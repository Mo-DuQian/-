`include "lib/defines.vh"
module MEM(
    input wire clk,
    input wire rst,
    // input wire flush,
    input wire [`StallBus-1:0] stall,

    input wire [`EX_TO_MEM_WD-1:0] ex_to_mem_bus,
    input wire [31:0]data_sram_rdata,

    output wire [`MEM_TO_WB_WD-1:0] mem_to_wb_bus,
    output wire [4:0] cf_mem_addr,
    output wire [31:0] cf_mem_data,
    output wire cf_mem_we,
    
    output wire cf_mem_we_hi,
    output wire cf_mem_we_lo,
    output wire [31:0] cf_mem_hi,
    output wire [31:0] cf_mem_lo
);

    reg [`EX_TO_MEM_WD-1:0] ex_to_mem_bus_r;

    always @ (posedge clk) begin
        if (rst) begin
            ex_to_mem_bus_r <= `EX_TO_MEM_WD'b0;
        end
        // else if (flush) begin
        //     ex_to_mem_bus_r <= `EX_TO_MEM_WD'b0;
        // end
        else if (stall[3]==`Stop && stall[4]==`NoStop) begin
            ex_to_mem_bus_r <= `EX_TO_MEM_WD'b0;
        end
        else if (stall[3]==`NoStop) begin
            ex_to_mem_bus_r <= ex_to_mem_bus;
        end
    end

    wire [31:0] mem_pc;
    wire data_ram_en;
    wire [3:0] data_ram_wen;
    wire sel_rf_res;
    wire rf_we;
    wire [4:0] rf_waddr;
    wire [31:0] rf_wdata;
    wire [31:0] ex_result;
    wire [31:0] mem_result;
    
    wire mem_we_hi;
    wire mem_we_lo;
    wire [31:0] mem_hi;
    wire [31:0] mem_lo;
    
    wire inst_lb,inst_lbu,inst_lh,inst_lhu;

    assign {
        inst_lb,
        inst_lbu,
        inst_lh,
        inst_lhu,
        mem_we_hi,
        mem_we_lo,
        mem_hi,
        mem_lo,
        mem_pc,         // 75:44
        data_ram_en,    // 43
        data_ram_wen,   // 42:39
        sel_rf_res,     // 38
        rf_we,          // 37
        rf_waddr,       // 36:32
        ex_result       // 31:0
    } =  ex_to_mem_bus_r;
    
    wire [3:0] data_sram_ren;
    assign data_sram_ren =  inst_lb & ex_result[1:0] == 2'b00 ? 4'b0001:
                            inst_lb & ex_result[1:0] == 2'b01 ? 4'b0010:
                            inst_lb & ex_result[1:0] == 2'b10 ? 4'b0100:
                            inst_lb & ex_result[1:0] == 2'b11 ? 4'b1000:
                            inst_lh & ex_result[1:0] == 2'b00 ? 4'b0011:
                            inst_lh & ex_result[1:0] == 2'b10 ? 4'b1100:
                            inst_lbu & ex_result[1:0] == 2'b00 ? 4'b0001:
                            inst_lbu & ex_result[1:0] == 2'b01 ? 4'b0010:
                            inst_lbu & ex_result[1:0] == 2'b10 ? 4'b0100:
                            inst_lbu & ex_result[1:0] == 2'b11 ? 4'b1000:
                            inst_lhu & ex_result[1:0] == 2'b00 ? 4'b0011:
                            inst_lhu & ex_result[1:0] == 2'b10 ? 4'b1100:
                            4'b1111;
    
    
    //assign mem_result = data_sram_rdata;
//    assign mem_result =    inst_lbu & data_sram_ren==4'b0001 ? {24'b0,data_sram_rdata[7:0]}:
//                           inst_lbu &  data_sram_ren==4'b0010 ? {16'b0,data_sram_rdata[15:8],8'b0}:
//                           inst_lbu &  data_sram_ren==4'b0100 ? {8'b0,data_sram_rdata[23:16],16'b0}:
//                           inst_lbu &  data_sram_ren==4'b1000 ? {data_sram_rdata[31:24],24'b0}:
//                           inst_lhu &  data_sram_ren==4'b0011 ? {16'b0,data_sram_rdata[15:0]}:
//                           inst_lhu &  data_sram_ren==4'b1100 ? {data_sram_rdata[31:16],16'b0}:
//                           inst_lb &  data_sram_ren==4'b0001 ? {{24{data_sram_rdata[7]}},data_sram_rdata[7:0]}:
//                           inst_lb &  data_sram_ren==4'b0010 ? {{16{data_sram_rdata[15]}},data_sram_rdata[15:8],8'b0}:
//                           inst_lb &  data_sram_ren==4'b0100 ? {{8{data_sram_rdata[23]}},data_sram_rdata[23:16],16'b0}:
//                           inst_lb &  data_sram_ren==4'b1000 ? {data_sram_rdata[31:24],24'b0}:
//                           inst_lh &  data_sram_ren==4'b0011 ? {{16{data_sram_rdata[15]}},data_sram_rdata[15:0]}:
//                           inst_lh &  data_sram_ren==4'b1100 ? {data_sram_rdata[31:16],16'b0}:
//                           data_sram_rdata;
                           
    assign mem_result =    inst_lbu & data_sram_ren==4'b0001 ? {24'b0,data_sram_rdata[7:0]}:
                           inst_lbu &  data_sram_ren==4'b0010 ? {24'b0,data_sram_rdata[15:8]}:
                           inst_lbu &  data_sram_ren==4'b0100 ? {24'b0,data_sram_rdata[23:16]}:
                           inst_lbu &  data_sram_ren==4'b1000 ? {24'b0,data_sram_rdata[31:24]}:
                           inst_lhu &  data_sram_ren==4'b0011 ? {16'b0,data_sram_rdata[15:0]}:
                           inst_lhu &  data_sram_ren==4'b1100 ? {16'b0,data_sram_rdata[31:16]}:
                           inst_lb &  data_sram_ren==4'b0001 ? {{24{data_sram_rdata[7]}},data_sram_rdata[7:0]}:
                           inst_lb &  data_sram_ren==4'b0010 ? {{24{data_sram_rdata[15]}},data_sram_rdata[15:8]}:
                           inst_lb &  data_sram_ren==4'b0100 ? {{24{data_sram_rdata[23]}},data_sram_rdata[23:16]}:
                           inst_lb &  data_sram_ren==4'b1000 ? {{24{data_sram_rdata[31]}}, data_sram_rdata[31:24]}:
                           inst_lh &  data_sram_ren==4'b0011 ? {{16{data_sram_rdata[15]}},data_sram_rdata[15:0]}:
                           inst_lh &  data_sram_ren==4'b1100 ? {{16{data_sram_rdata[31]}},data_sram_rdata[31:16]}:
                           data_sram_rdata;


    assign rf_wdata = sel_rf_res ? mem_result : ex_result;
    
    assign cf_mem_addr = rf_waddr ;
    assign cf_mem_we = rf_we;
    assign cf_mem_data = rf_wdata;
    
    assign cf_mem_we_hi = mem_we_hi;
    assign cf_mem_we_lo = mem_we_lo;
    assign cf_mem_hi=mem_hi;
    assign cf_mem_lo=mem_lo;

    assign mem_to_wb_bus = {
         mem_we_hi,
        mem_we_lo,
        mem_hi,
        mem_lo,
        mem_pc,     // 41:38
        rf_we,      // 37
        rf_waddr,   // 36:32
        rf_wdata    // 31:0
    };




endmodule