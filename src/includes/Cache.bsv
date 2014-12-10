import Types::*;
import MemTypes::*;
import CacheTypes::*;
import MemUtil::*;
import Vector::*;
import Fifo::*;

import SACache::*;

typedef enum { Ready, StartMiss, SendFillReq, WaitFillResp } CacheStatus deriving ( Bits, Eq );

module mkTranslator( WideMem mem, Cache ifc );
    
    Fifo#( 2, CacheWordSelect ) off <- mkCFFifo;
    
    function CacheWordSelect getOff( Addr addr ) = truncate( addr >> 2 );
    
    method Action req( MemReq r );
        if( r.op == Ld ) off.enq( getOff( r.addr ) );
        mem.req( toWideMemReq( r ) );
    endmethod
    
    method ActionValue#( MemResp ) resp;
        let cl <- mem.resp; off.deq;
        return cl[ off.first ];
    endmethod
    
endmodule

module mkCache( WideMem _mem, Cache ifc );
    
    L2Cache mem <- mkSACache( _mem, True );
    
    Vector#( CacheRows, Reg#( CacheLine ) )          datArr <- replicateM( mkReg( replicate( 0 ) ) );
    Vector#( CacheRows, Reg#( Maybe#( CacheTag ) ) ) tagArr <- replicateM( mkReg( tagged Invalid ) );
    Vector#( CacheRows, Reg#( Bool ) )               drtArr <- replicateM( mkReg( False ) );
    
    Fifo#( 2, Data ) hitQ <- mkCFFifo;
    
    Reg#( MemReq )      missReq <- mkRegU;
    Reg#( CacheStatus ) status  <- mkReg( Ready );
    
    function CacheIndex      getIdx( Addr addr ) = truncate( addr >> valueOf( TLog#( CacheLineBytes ) ) );
    function CacheWordSelect getOff( Addr addr ) = truncate( addr >> 2 );
    function CacheTag        getTag( Addr addr ) = truncateLSB( addr );
    
    // Update Memory: Write-Back
    rule startMiss( status == StartMiss );
        
        let idx = getIdx( missReq.addr );
        let tag = tagArr[ idx ];
        
        if( isValid( tag ) && drtArr[ idx ] ) begin
            Addr addr = { fromMaybe( ?, tag ), idx, '0 };
            mem.req( WideMemReq{ write_en: '1, addr: addr, data: datArr[ idx ] } );
        end
        
        status <= SendFillReq;
        
    endrule
    
    // Read DRAM
    rule sendFillReq( status == SendFillReq );
        let r = toWideMemReq( missReq ); r.write_en = '0;
        mem.req( r );
        status <= WaitFillResp;
    endrule
    
    // Update Cache
    rule waitFillResp( status == WaitFillResp );
        
        let off = getOff( missReq.addr );
        let idx = getIdx( missReq.addr );
        let tag = getTag( missReq.addr );
        let st  = missReq.op == St;
        let cl <- mem.resp;
        
        if( st ) cl[ off ] = missReq.data;
        else hitQ.enq( cl[ off ] );
        
        datArr[ idx ] <= cl;
        tagArr[ idx ] <= tagged Valid tag;
        drtArr[ idx ] <= st;
        
        status <= Ready;
        
    endrule
    
    method Action req( MemReq r ) if( status == Ready );
        
        let off  = getOff( r.addr );
        let idx  = getIdx( r.addr );
        let tag  = getTag( r.addr );
        let cTag = tagArr[ idx ];
        let hit  = isValid( cTag ) ? fromMaybe( ?, cTag ) == tag : False;
        
        //$fwrite( stderr, "pc: %x; tag: %x; idx: %x; off: %x; hit: %x\n", r.addr, tag, idx, off, hit );
        
        if( hit ) begin
            let cl = datArr[ idx ];
            if( r.op == Ld ) hitQ.enq( cl[ off ] );
            else begin
                cl[ off ] = r.data;
                datArr[ idx ] <= cl;
                drtArr[ idx ] <= True;
            end
        end else begin
            missReq <= r;
            status  <= StartMiss;
        end
    
    endmethod
    
    method ActionValue#( Data ) resp;
        hitQ.deq;
        return hitQ.first;
    endmethod
    
endmodule
