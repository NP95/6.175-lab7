import Types::*;
import ProcTypes::*;
import CacheTypes::*;
import Fifo::*;
import Vector::*;

typedef 64 SACacheSize;
typedef 4 NumWays;
typedef TDiv#( SACacheSize, NumWays ) NumRows;

typedef TLog#( NumWays ) NumWayBits;
typedef TLog#( NumRows ) NumRowBits;
typedef TSub#( 26, NumRowBits ) NumCacheTagBits;

typedef Bit#( NumCacheTagBits ) SACacheTag;
typedef Bit#( NumWayBits ) WayIdx;
typedef Bit#( NumRowBits ) RowIdx;
typedef WayIdx Age;

typedef enum { Ready, WriteBack, SendFillReq, WaitFillResp } SACacheStatus deriving ( Bits, Eq );

interface L2Cache;
    method Action req( WideMemReq r );
    method ActionValue#( CacheLine ) resp;
endinterface

module mkSACache( WideMem mem, Bool wb, L2Cache ifc );
    
    Vector#( NumWays, Vector#( NumRows, Reg#( SACacheTag ) ) )
        tag <- replicateM( replicateM( mkReg( 0 ) ) );
    
    Vector#( NumWays, Vector#( NumRows, Reg#( CacheLine ) ) )
        data <- replicateM( replicateM( mkReg( replicate( 0 ) ) ) );
    
    Vector#( NumWays, Vector#( NumRows, Reg#( Bool ) ) )
        dirty <- replicateM( replicateM( mkReg( False ) ) );
    
    Vector#( NumWays, Vector#( NumRows, Reg#( Age ) ) )
        age <- replicateM( replicateM( mkReg( '1 ) ) );
    
    Fifo#( 2, CacheLine ) hitQ <- mkCFFifo;
    
    Reg#( WideMemReq ) missReq <- mkRegU;
    Reg#( SACacheStatus ) status <- mkReg( Ready );
    Reg#( WayIdx ) lru <- mkReg( 0 );
    
    function SACacheTag getSACacheTag( Addr a ) = truncateLSB( a );
    function RowIdx getRow( Addr a ) = truncateLSB( a << valueOf( NumCacheTagBits ) );
    function Bit#( 26 ) getDDR3Addr( Addr a ) = truncate( a >> 6 );
    
    function WayIdx findLRU( RowIdx r );
        WayIdx w = 0;
        for ( Integer i = 0; i < valueOf( NumWays ); i = i + 1 )
            if ( age[ fromInteger( i ) ][ r ] == '1 )
                w = fromInteger( i );
        return w;
    endfunction
    
    function Maybe#( WayIdx ) searchTag( SACacheTag t, RowIdx r );
        Maybe#( WayIdx ) w = tagged Invalid;
        for ( Integer i = 0; i < valueOf( NumWays ); i = i + 1 )
            if ( tag[ fromInteger( i ) ][ r ] == t )
                w = tagged Valid fromInteger( i );
        return w;
    endfunction
    
    function Action zeroAge( WayIdx w, RowIdx r );
        return (action
            age[ w ][ r ] <= 0;
            for ( Integer i = 0; i < valueOf( NumWays ); i = i + 1 )
                if ( age[ fromInteger( i ) ][ r ] < age[ w ][ r ] )
                    age[ fromInteger( i ) ][ r ] <= age[ fromInteger( i ) ][ r ] + 1;
        endaction);
    endfunction
    
    rule writeBack( status == WriteBack );
        
        let r = getRow( missReq.addr );
        
        if( dirty[ lru ][ r ] ) mem.req( WideMemReq{
            write_en: '1,
            addr: { tag[ lru ][ r ], r, 0 },
            data: data[ lru ][ r ]
        } );
        
        status <= SendFillReq;
        
    endrule
    
    rule sendFillReq( status == SendFillReq );
        let r = missReq; r.write_en = 0;
        mem.req( r );
        status <= WaitFillResp;
    endrule
    
    rule waitFillResp( status == WaitFillResp );
        
        let t  = getSACacheTag( missReq.addr );
        let r  = getRow( missReq.addr );
        let st = missReq.write_en != 0;
        let d <- mem.resp;
        
        if( st ) d = missReq.data;
        else hitQ.enq( d );
        
        tag  [ lru ][ r ] <= t;
        data [ lru ][ r ] <= d;
        dirty[ lru ][ r ] <= st;
        
        zeroAge( lru, r );
        
        status <= Ready;
        
    endrule
    
    method Action req( WideMemReq memReq ) if( status == Ready );
        let t = getSACacheTag( memReq.addr );
        let r = getRow( memReq.addr );
        if( searchTag( t, r ) matches tagged Valid .w ) begin
            if( memReq.write_en == 0 ) hitQ.enq( data[ w ][ r ] );
            else begin
                data [ w ][ r ] <= memReq.data;
                dirty[ w ][ r ] <= True;
                if( !wb ) mem.req( memReq );
            end
            zeroAge( w, r );
        end else begin
            lru     <= findLRU( r );
            missReq <= memReq;
            if( wb ) status <= WriteBack; else status <= SendFillReq;
        end
    endmethod
    
    method ActionValue#( CacheLine ) resp;
        hitQ.deq;
        return hitQ.first;
    endmethod
    
endmodule

