//`include "defines.vh"
module hilo(
    input wire clk,
   
    output wire [31:0] r_hi,
    output wire [31:0] r_lo,
    
    input wire we_hi,
    input wire we_lo,
    input wire [31:0] wb_hi,
    input wire [31:0] wb_lo
);
    reg [31:0] hi;
    reg [31:0] lo;
    // write
    always @ (posedge clk) begin
        if (we_hi) begin
            hi <= wb_hi;
        end
        if (we_lo) begin
            lo <= wb_lo;
        end
    end

    // read out 1
    assign r_hi = hi;

    // read out2
    assign r_lo = lo;
endmodule