import Types::*;
import ProcTypes::*;
import CacheTypes::*;
import Fifo::*;
import Vector::*;

typedef 16 SACacheSize;
typedef 4 NumSets;
typedef TDiv#( SACacheSize, NumSets ) NumSlots;

typedef TSub#( 26, NumSetBits ) NumCacheTagBits;
typedef TLog#( NumSets ) NumSetBits;
typedef TLog#( NumSlots ) NumSlotBits;

typedef Bit#( NumCacheTagBits ) SACacheTag;
typedef Bit#( NumSetBits ) SetIdx;
typedef Bit#( NumSlotBits ) SlotIdx;
typedef SlotIdx Age;

typedef enum { Ready, WriteBack, SendFillReq, WaitFillResp } SACacheStatus deriving ( Bits, Eq );

interface L2Cache;
    method Action req( WideMemReq r );
    method ActionValue#( CacheLine ) resp;
endinterface

module mkSACache( WideMem mem, L2Cache ifc );
    
    Vector#( NumSets, Vector#( NumSlots, Reg#( SACacheTag ) ) )
        tag <- replicateM( replicateM( mkRegU ) );
    
    Vector#( NumSets, Vector#( NumSlots, Reg#( CacheLine ) ) )
        data <- replicateM( replicateM( mkRegU ) );
    
    Vector#( NumSets, Vector#( NumSlots, Reg#( Bool ) ) )
        dirty <- replicateM( replicateM( mkReg( False ) ) );
    
    Vector#( NumSets, Vector#( NumSlots, Reg#( Age ) ) )
        age <- replicateM( replicateM( mkReg( '1 ) ) );
    
    Fifo#( 2, CacheLine ) hitQ <- mkCFFifo;
    
    Reg#( WideMemReq ) missReq <- mkRegU;
    Reg#( SACacheStatus ) status <- mkReg( Ready );
    Reg#( SlotIdx ) lru <- mkRegU;
    
    function SACacheTag getSACacheTag( Addr a ) = truncateLSB( a );
    function SetIdx getSetIdx( Addr a ) = truncateLSB( a << valueOf( NumCacheTagBits ) );
    
    function SlotIdx getLRUSlotIdx( SetIdx s );
        SlotIdx idx = 0;
        for( Integer i = 0; i < valueOf( NumSlots ); i = i + 1 ) begin
            if( age[ s ][ fromInteger( i ) ] == '1 ) idx = fromInteger( i );
        end
        return idx;
    endfunction
    
    function Maybe#( SlotIdx ) getTagSlotIdx( SACacheTag t, SetIdx s );
        Maybe#( SlotIdx ) l = tagged Invalid;
        for( Integer i = 0; i < valueOf( NumSlots ); i = i + 1 )
            if( tag[ s ][ fromInteger( i ) ] == t ) l = tagged Valid fromInteger( i );
        return l;
    endfunction
    
    function Action zeroAge( SetIdx s, SlotIdx l );
        return ( action
            age[ s ][ l ] <= 0;
            for( Integer i = 0; i < valueOf( NumSlots ); i = i + 1 )
                if( age[ s ][ fromInteger( i ) ] < age[ s ][ l ] )
                    age[ s ][ fromInteger( i ) ] <= age[ s ][ fromInteger( i ) ] + 1;
        endaction );
    endfunction
    
    rule writeBack( status == WriteBack );
        
        let t = getSACacheTag( missReq.addr );
        let s = getSetIdx( missReq.addr );
        
        if( dirty[ s ][ lru ] ) mem.req( WideMemReq{
            write_en: '1,
            addr: { tag[ s ][ lru ], s, 0 },
            data: data[ s ][ lru ]
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
        let s  = getSetIdx( missReq.addr );
        let ld = missReq.write_en == 0;
        let d <- mem.resp;
        
        if( ld ) hitQ.enq( d );
        else d = missReq.data;
        
        tag  [ s ][ lru ] <= t;
        data [ s ][ lru ] <= d;
        dirty[ s ][ lru ] <= !ld;
        
        zeroAge( s, lru );
        
        status <= Ready;
        
    endrule
    
    method Action req( WideMemReq r ) if( status == Ready );
        let t = getSACacheTag( r.addr );
        let s = getSetIdx( r.addr );
        if( getTagSlotIdx( t, s ) matches tagged Valid .l ) begin
            if( r.write_en == 0 ) hitQ.enq( data[ s ][ l ] );
            else begin
                data [ s ][ l ] <= r.data;
                dirty[ s ][ l ] <= True;
            end
            zeroAge( s, l );
        end else begin
            lru     <= getLRUSlotIdx( s );
            missReq <= r;
            status  <= WriteBack;
        end
    endmethod
    
    method ActionValue#( CacheLine ) resp;
        hitQ.deq;
        return hitQ.first;
    endmethod
    
endmodule

