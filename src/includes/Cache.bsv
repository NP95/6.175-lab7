import Types::*;
import MemTypes::*;
import CacheTypes::*;
import MemUtil::*;
import Vector::*;
import Fifo::*;

typedef enum { Ready, StartMiss, SendFillReq, WaitFillResp } CacheStatus deriving ( Bits, Eq );

module mkTranslator( WideMem mem, Cache ifc );
    
    Fifo#( 2, CacheWordSelect ) wordSel <- mkCFFifo;
    
    function CacheWordSelect getOff( Addr addr ) = truncate( addr >> 2 );
    
    method Action req( MemReq r ) if( wordSel.notFull );
        if( r.op == Ld ) wordSel.enq( getOff( r.addr ) );
        mem.req( toWideMemReq( r ) );
    endmethod
    
    method ActionValue#( MemResp ) resp if( wordSel.notEmpty );
        let cl <- mem.resp; wordSel.deq;
        return cl[ wordSel.first ];
    endmethod
    
endmodule

module mkCache( WideMem mem, Cache ifc );
    
    Vector#( TLog#( CacheRows ), Reg#( CacheLine ) )          datArr <- replicateM( mkReg( replicate( 0 ) ) );
    Vector#( TLog#( CacheRows ), Reg#( Maybe#( CacheTag ) ) ) tagArr <- replicateM( mkReg( tagged Invalid ) );
    Vector#( TLog#( CacheRows ), Reg#( Bool ) )               drtArr <- replicateM( mkReg( False ) );
    
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
        let drt = drtArr[ idx ];
        
        if( isValid( tag ) && drt ) begin
            let addr = { fromMaybe( ?, tag ), idx, 6'b0 }; // 6 is really TLog#(CacheLineBytes)
            let data = datArr[ idx ];
            mem.req( WideMemReq{ write_en: '1, addr: addr, data: data } );
        end
        
        status <= SendFillReq;
        
    endrule
    
    // Read Memory
    rule sendFillReq( status == SendFillReq );
        mem.req( toWideMemReq( missReq ) );
        status <= WaitFillResp;
    endrule
    
    // Update Cache
    rule waitFillResp( status == WaitFillResp && hitQ.notFull );
        
        let off = getOff( missReq.addr );
        let idx = getIdx( missReq.addr );
        let tag = getTag( missReq.addr );
        let dat = datArr[ idx ];
        let st  = missReq.op == St;
        
        if( st ) dat[ off ] = missReq.data;
        else begin
            dat <- mem.resp;
            hitQ.enq( dat[ off ] );
        end
        
        datArr[ idx ] <= dat;
        tagArr[ idx ] <= Valid( tag );
        drtArr[ idx ] <= st;
        
        status <= Ready;
        
    endrule
    
    method Action req( MemReq r ) if( status == Ready );
        
        let off  = getOff( r.addr );
        let idx  = getIdx( r.addr );
        let tag  = getTag( r.addr );
        let cTag = tagArr[ idx ];
        let hit  = isValid( cTag ) ? fromMaybe( ?, cTag ) == tag : False;
        
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
    
    method ActionValue#( Data ) resp if( hitQ.notEmpty );
        hitQ.deq;
        return hitQ.first;
    endmethod
    
endmodule
