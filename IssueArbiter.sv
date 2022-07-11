// Generator : SpinalHDL v1.7.0a    git head : 150a9b9067020722818dfb17df4a23ac712a7af8
// Component : IssueArbiter
// Git hash  : 4af8a6166e1bebc1080310e4c12ccdda677eccbc

`timescale 1ns/1ps

module IssueArbiter (
  input      [6:0]    io_instsInfo_0_robAddr,
  input      [3:0]    io_instsInfo_0_issueQueneIdx,
  input               io_instsInfo_0_ready,
  input      [6:0]    io_instsInfo_1_robAddr,
  input      [3:0]    io_instsInfo_1_issueQueneIdx,
  input               io_instsInfo_1_ready,
  input      [6:0]    io_instsInfo_2_robAddr,
  input      [3:0]    io_instsInfo_2_issueQueneIdx,
  input               io_instsInfo_2_ready,
  input      [6:0]    io_instsInfo_3_robAddr,
  input      [3:0]    io_instsInfo_3_issueQueneIdx,
  input               io_instsInfo_3_ready,
  input      [6:0]    io_instsInfo_4_robAddr,
  input      [3:0]    io_instsInfo_4_issueQueneIdx,
  input               io_instsInfo_4_ready,
  input      [6:0]    io_instsInfo_5_robAddr,
  input      [3:0]    io_instsInfo_5_issueQueneIdx,
  input               io_instsInfo_5_ready,
  input      [6:0]    io_instsInfo_6_robAddr,
  input      [3:0]    io_instsInfo_6_issueQueneIdx,
  input               io_instsInfo_6_ready,
  input      [6:0]    io_instsInfo_7_robAddr,
  input      [3:0]    io_instsInfo_7_issueQueneIdx,
  input               io_instsInfo_7_ready,
  input      [6:0]    io_instsInfo_8_robAddr,
  input      [3:0]    io_instsInfo_8_issueQueneIdx,
  input               io_instsInfo_8_ready,
  input      [6:0]    io_instsInfo_9_robAddr,
  input      [3:0]    io_instsInfo_9_issueQueneIdx,
  input               io_instsInfo_9_ready,
  input      [6:0]    io_instsInfo_10_robAddr,
  input      [3:0]    io_instsInfo_10_issueQueneIdx,
  input               io_instsInfo_10_ready,
  input      [6:0]    io_instsInfo_11_robAddr,
  input      [3:0]    io_instsInfo_11_issueQueneIdx,
  input               io_instsInfo_11_ready,
  input      [6:0]    io_instsInfo_12_robAddr,
  input      [3:0]    io_instsInfo_12_issueQueneIdx,
  input               io_instsInfo_12_ready,
  input      [6:0]    io_instsInfo_13_robAddr,
  input      [3:0]    io_instsInfo_13_issueQueneIdx,
  input               io_instsInfo_13_ready,
  input      [6:0]    io_instsInfo_14_robAddr,
  input      [3:0]    io_instsInfo_14_issueQueneIdx,
  input               io_instsInfo_14_ready,
  input      [6:0]    io_instsInfo_15_robAddr,
  input      [3:0]    io_instsInfo_15_issueQueneIdx,
  input               io_instsInfo_15_ready,
  output reg [6:0]    io_issueOkInst_robAddr,
  output reg [3:0]    io_issueOkInst_issueQueneIdx,
  output reg          io_issueOkInst_ready,
  input               clk,
  input               reset
);

  reg        [6:0]    _zz_io_issueOkInst_robAddr;
  reg        [3:0]    _zz_io_issueOkInst_issueQueneIdx;
  wire                _zz_io_issueOkInst_ready;
  reg        [6:0]    _zz_io_issueOkInst_robAddr_1;
  reg        [3:0]    _zz_io_issueOkInst_issueQueneIdx_1;
  wire                _zz_io_issueOkInst_ready_1;
  reg        [6:0]    _zz_io_issueOkInst_robAddr_2;
  reg        [3:0]    _zz_io_issueOkInst_issueQueneIdx_2;
  wire                _zz_io_issueOkInst_ready_2;
  reg        [6:0]    _zz_io_issueOkInst_robAddr_3;
  reg        [3:0]    _zz_io_issueOkInst_issueQueneIdx_3;
  wire                _zz_io_issueOkInst_ready_3;
  reg        [6:0]    _zz_io_issueOkInst_robAddr_4;
  reg        [3:0]    _zz_io_issueOkInst_issueQueneIdx_4;
  wire                _zz_io_issueOkInst_ready_4;
  reg        [6:0]    _zz_io_issueOkInst_robAddr_5;
  reg        [3:0]    _zz_io_issueOkInst_issueQueneIdx_5;
  wire                _zz_io_issueOkInst_ready_5;
  reg        [6:0]    _zz_io_issueOkInst_robAddr_6;
  reg        [3:0]    _zz_io_issueOkInst_issueQueneIdx_6;
  wire                _zz_io_issueOkInst_ready_6;
  reg        [6:0]    _zz_io_issueOkInst_robAddr_7;
  reg        [3:0]    _zz_io_issueOkInst_issueQueneIdx_7;
  wire                _zz_io_issueOkInst_ready_7;
  wire                _zz_io_issueOkInst_robAddr_8;
  wire                _zz_io_issueOkInst_robAddr_9;
  wire       [6:0]    _zz_io_issueOkInst_robAddr_10;
  wire       [3:0]    _zz_io_issueOkInst_issueQueneIdx_8;
  wire       [1:0]    switch_IssueArbiter_l48;
  wire                _zz_io_issueOkInst_robAddr_11;
  wire                _zz_io_issueOkInst_robAddr_12;
  wire       [6:0]    _zz_io_issueOkInst_robAddr_13;
  wire       [3:0]    _zz_io_issueOkInst_issueQueneIdx_9;
  wire       [1:0]    switch_IssueArbiter_l48_1;
  wire                _zz_io_issueOkInst_robAddr_14;
  wire                _zz_io_issueOkInst_robAddr_15;
  wire       [6:0]    _zz_io_issueOkInst_robAddr_16;
  wire       [3:0]    _zz_io_issueOkInst_issueQueneIdx_10;
  wire       [1:0]    switch_IssueArbiter_l48_2;
  wire                _zz_io_issueOkInst_robAddr_17;
  wire                _zz_io_issueOkInst_robAddr_18;
  wire       [6:0]    _zz_io_issueOkInst_robAddr_19;
  wire       [3:0]    _zz_io_issueOkInst_issueQueneIdx_11;
  wire       [1:0]    switch_IssueArbiter_l48_3;
  wire                _zz_io_issueOkInst_robAddr_20;
  wire                _zz_io_issueOkInst_robAddr_21;
  wire       [6:0]    _zz_io_issueOkInst_robAddr_22;
  wire       [3:0]    _zz_io_issueOkInst_issueQueneIdx_12;
  wire       [1:0]    switch_IssueArbiter_l48_4;
  wire                _zz_io_issueOkInst_robAddr_23;
  wire                _zz_io_issueOkInst_robAddr_24;
  wire       [6:0]    _zz_io_issueOkInst_robAddr_25;
  wire       [3:0]    _zz_io_issueOkInst_issueQueneIdx_13;
  wire       [1:0]    switch_IssueArbiter_l48_5;
  wire                _zz_io_issueOkInst_robAddr_26;
  wire                _zz_io_issueOkInst_robAddr_27;
  wire       [6:0]    _zz_io_issueOkInst_robAddr_28;
  wire       [3:0]    _zz_io_issueOkInst_issueQueneIdx_14;
  wire       [1:0]    switch_IssueArbiter_l48_6;
  wire                _zz_io_issueOkInst_robAddr_29;
  wire                _zz_io_issueOkInst_robAddr_30;
  wire       [6:0]    _zz_io_issueOkInst_robAddr_31;
  wire       [3:0]    _zz_io_issueOkInst_issueQueneIdx_15;
  wire       [1:0]    switch_IssueArbiter_l48_7;
  reg        [6:0]    _zz_io_issueOkInst_robAddr_32;
  reg        [3:0]    _zz_io_issueOkInst_issueQueneIdx_16;
  wire                _zz_io_issueOkInst_ready_8;
  reg        [6:0]    _zz_io_issueOkInst_robAddr_33;
  reg        [3:0]    _zz_io_issueOkInst_issueQueneIdx_17;
  wire                _zz_io_issueOkInst_ready_9;
  reg        [6:0]    _zz_io_issueOkInst_robAddr_34;
  reg        [3:0]    _zz_io_issueOkInst_issueQueneIdx_18;
  wire                _zz_io_issueOkInst_ready_10;
  reg        [6:0]    _zz_io_issueOkInst_robAddr_35;
  reg        [3:0]    _zz_io_issueOkInst_issueQueneIdx_19;
  wire                _zz_io_issueOkInst_ready_11;
  wire                _zz_io_issueOkInst_robAddr_36;
  wire                _zz_io_issueOkInst_robAddr_37;
  wire       [6:0]    _zz_io_issueOkInst_robAddr_38;
  wire       [3:0]    _zz_io_issueOkInst_issueQueneIdx_20;
  wire       [1:0]    switch_IssueArbiter_l48_8;
  wire                _zz_io_issueOkInst_robAddr_39;
  wire                _zz_io_issueOkInst_robAddr_40;
  wire       [6:0]    _zz_io_issueOkInst_robAddr_41;
  wire       [3:0]    _zz_io_issueOkInst_issueQueneIdx_21;
  wire       [1:0]    switch_IssueArbiter_l48_9;
  wire                _zz_io_issueOkInst_robAddr_42;
  wire                _zz_io_issueOkInst_robAddr_43;
  wire       [6:0]    _zz_io_issueOkInst_robAddr_44;
  wire       [3:0]    _zz_io_issueOkInst_issueQueneIdx_22;
  wire       [1:0]    switch_IssueArbiter_l48_10;
  wire                _zz_io_issueOkInst_robAddr_45;
  wire                _zz_io_issueOkInst_robAddr_46;
  wire       [6:0]    _zz_io_issueOkInst_robAddr_47;
  wire       [3:0]    _zz_io_issueOkInst_issueQueneIdx_23;
  wire       [1:0]    switch_IssueArbiter_l48_11;
  reg        [6:0]    _zz_io_issueOkInst_robAddr_48;
  reg        [3:0]    _zz_io_issueOkInst_issueQueneIdx_24;
  wire                _zz_io_issueOkInst_ready_12;
  reg        [6:0]    _zz_io_issueOkInst_robAddr_49;
  reg        [3:0]    _zz_io_issueOkInst_issueQueneIdx_25;
  wire                _zz_io_issueOkInst_ready_13;
  wire                _zz_io_issueOkInst_robAddr_50;
  wire                _zz_io_issueOkInst_robAddr_51;
  wire       [6:0]    _zz_io_issueOkInst_robAddr_52;
  wire       [3:0]    _zz_io_issueOkInst_issueQueneIdx_26;
  wire       [1:0]    switch_IssueArbiter_l48_12;
  wire                _zz_io_issueOkInst_robAddr_53;
  wire                _zz_io_issueOkInst_robAddr_54;
  wire       [6:0]    _zz_io_issueOkInst_robAddr_55;
  wire       [3:0]    _zz_io_issueOkInst_issueQueneIdx_27;
  wire       [1:0]    switch_IssueArbiter_l48_13;
  reg        [6:0]    _zz_io_issueOkInst_robAddr_56;
  reg        [3:0]    _zz_io_issueOkInst_issueQueneIdx_28;
  wire                _zz_io_issueOkInst_robAddr_57;
  wire                _zz_io_issueOkInst_robAddr_58;
  wire       [6:0]    _zz_io_issueOkInst_robAddr_59;
  wire       [3:0]    _zz_io_issueOkInst_issueQueneIdx_29;
  wire       [1:0]    switch_IssueArbiter_l48_14;

  assign _zz_io_issueOkInst_robAddr_8 = (io_instsInfo_0_robAddr[5 : 0] < io_instsInfo_1_robAddr[5 : 0]);
  assign _zz_io_issueOkInst_robAddr_9 = ((io_instsInfo_0_robAddr[6] == io_instsInfo_1_robAddr[6]) ? _zz_io_issueOkInst_robAddr_8 : (! _zz_io_issueOkInst_robAddr_8));
  assign _zz_io_issueOkInst_robAddr_10 = (_zz_io_issueOkInst_robAddr_9 ? io_instsInfo_0_robAddr : io_instsInfo_1_robAddr);
  assign _zz_io_issueOkInst_issueQueneIdx_8 = (_zz_io_issueOkInst_robAddr_9 ? io_instsInfo_0_issueQueneIdx : io_instsInfo_1_issueQueneIdx);
  assign switch_IssueArbiter_l48 = {io_instsInfo_0_ready,io_instsInfo_1_ready};
  always @(*) begin
    case(switch_IssueArbiter_l48)
      2'b11 : begin
        _zz_io_issueOkInst_robAddr = _zz_io_issueOkInst_robAddr_10;
      end
      2'b10 : begin
        _zz_io_issueOkInst_robAddr = io_instsInfo_0_robAddr;
      end
      2'b01 : begin
        _zz_io_issueOkInst_robAddr = io_instsInfo_1_robAddr;
      end
      default : begin
        _zz_io_issueOkInst_robAddr = _zz_io_issueOkInst_robAddr_10;
      end
    endcase
  end

  always @(*) begin
    case(switch_IssueArbiter_l48)
      2'b11 : begin
        _zz_io_issueOkInst_issueQueneIdx = _zz_io_issueOkInst_issueQueneIdx_8;
      end
      2'b10 : begin
        _zz_io_issueOkInst_issueQueneIdx = io_instsInfo_0_issueQueneIdx;
      end
      2'b01 : begin
        _zz_io_issueOkInst_issueQueneIdx = io_instsInfo_1_issueQueneIdx;
      end
      default : begin
        _zz_io_issueOkInst_issueQueneIdx = _zz_io_issueOkInst_issueQueneIdx_8;
      end
    endcase
  end

  assign _zz_io_issueOkInst_ready = (io_instsInfo_0_ready || io_instsInfo_1_ready);
  assign _zz_io_issueOkInst_robAddr_11 = (io_instsInfo_2_robAddr[5 : 0] < io_instsInfo_3_robAddr[5 : 0]);
  assign _zz_io_issueOkInst_robAddr_12 = ((io_instsInfo_2_robAddr[6] == io_instsInfo_3_robAddr[6]) ? _zz_io_issueOkInst_robAddr_11 : (! _zz_io_issueOkInst_robAddr_11));
  assign _zz_io_issueOkInst_robAddr_13 = (_zz_io_issueOkInst_robAddr_12 ? io_instsInfo_2_robAddr : io_instsInfo_3_robAddr);
  assign _zz_io_issueOkInst_issueQueneIdx_9 = (_zz_io_issueOkInst_robAddr_12 ? io_instsInfo_2_issueQueneIdx : io_instsInfo_3_issueQueneIdx);
  assign switch_IssueArbiter_l48_1 = {io_instsInfo_2_ready,io_instsInfo_3_ready};
  always @(*) begin
    case(switch_IssueArbiter_l48_1)
      2'b11 : begin
        _zz_io_issueOkInst_robAddr_1 = _zz_io_issueOkInst_robAddr_13;
      end
      2'b10 : begin
        _zz_io_issueOkInst_robAddr_1 = io_instsInfo_2_robAddr;
      end
      2'b01 : begin
        _zz_io_issueOkInst_robAddr_1 = io_instsInfo_3_robAddr;
      end
      default : begin
        _zz_io_issueOkInst_robAddr_1 = _zz_io_issueOkInst_robAddr_13;
      end
    endcase
  end

  always @(*) begin
    case(switch_IssueArbiter_l48_1)
      2'b11 : begin
        _zz_io_issueOkInst_issueQueneIdx_1 = _zz_io_issueOkInst_issueQueneIdx_9;
      end
      2'b10 : begin
        _zz_io_issueOkInst_issueQueneIdx_1 = io_instsInfo_2_issueQueneIdx;
      end
      2'b01 : begin
        _zz_io_issueOkInst_issueQueneIdx_1 = io_instsInfo_3_issueQueneIdx;
      end
      default : begin
        _zz_io_issueOkInst_issueQueneIdx_1 = _zz_io_issueOkInst_issueQueneIdx_9;
      end
    endcase
  end

  assign _zz_io_issueOkInst_ready_1 = (io_instsInfo_2_ready || io_instsInfo_3_ready);
  assign _zz_io_issueOkInst_robAddr_14 = (io_instsInfo_4_robAddr[5 : 0] < io_instsInfo_5_robAddr[5 : 0]);
  assign _zz_io_issueOkInst_robAddr_15 = ((io_instsInfo_4_robAddr[6] == io_instsInfo_5_robAddr[6]) ? _zz_io_issueOkInst_robAddr_14 : (! _zz_io_issueOkInst_robAddr_14));
  assign _zz_io_issueOkInst_robAddr_16 = (_zz_io_issueOkInst_robAddr_15 ? io_instsInfo_4_robAddr : io_instsInfo_5_robAddr);
  assign _zz_io_issueOkInst_issueQueneIdx_10 = (_zz_io_issueOkInst_robAddr_15 ? io_instsInfo_4_issueQueneIdx : io_instsInfo_5_issueQueneIdx);
  assign switch_IssueArbiter_l48_2 = {io_instsInfo_4_ready,io_instsInfo_5_ready};
  always @(*) begin
    case(switch_IssueArbiter_l48_2)
      2'b11 : begin
        _zz_io_issueOkInst_robAddr_2 = _zz_io_issueOkInst_robAddr_16;
      end
      2'b10 : begin
        _zz_io_issueOkInst_robAddr_2 = io_instsInfo_4_robAddr;
      end
      2'b01 : begin
        _zz_io_issueOkInst_robAddr_2 = io_instsInfo_5_robAddr;
      end
      default : begin
        _zz_io_issueOkInst_robAddr_2 = _zz_io_issueOkInst_robAddr_16;
      end
    endcase
  end

  always @(*) begin
    case(switch_IssueArbiter_l48_2)
      2'b11 : begin
        _zz_io_issueOkInst_issueQueneIdx_2 = _zz_io_issueOkInst_issueQueneIdx_10;
      end
      2'b10 : begin
        _zz_io_issueOkInst_issueQueneIdx_2 = io_instsInfo_4_issueQueneIdx;
      end
      2'b01 : begin
        _zz_io_issueOkInst_issueQueneIdx_2 = io_instsInfo_5_issueQueneIdx;
      end
      default : begin
        _zz_io_issueOkInst_issueQueneIdx_2 = _zz_io_issueOkInst_issueQueneIdx_10;
      end
    endcase
  end

  assign _zz_io_issueOkInst_ready_2 = (io_instsInfo_4_ready || io_instsInfo_5_ready);
  assign _zz_io_issueOkInst_robAddr_17 = (io_instsInfo_6_robAddr[5 : 0] < io_instsInfo_7_robAddr[5 : 0]);
  assign _zz_io_issueOkInst_robAddr_18 = ((io_instsInfo_6_robAddr[6] == io_instsInfo_7_robAddr[6]) ? _zz_io_issueOkInst_robAddr_17 : (! _zz_io_issueOkInst_robAddr_17));
  assign _zz_io_issueOkInst_robAddr_19 = (_zz_io_issueOkInst_robAddr_18 ? io_instsInfo_6_robAddr : io_instsInfo_7_robAddr);
  assign _zz_io_issueOkInst_issueQueneIdx_11 = (_zz_io_issueOkInst_robAddr_18 ? io_instsInfo_6_issueQueneIdx : io_instsInfo_7_issueQueneIdx);
  assign switch_IssueArbiter_l48_3 = {io_instsInfo_6_ready,io_instsInfo_7_ready};
  always @(*) begin
    case(switch_IssueArbiter_l48_3)
      2'b11 : begin
        _zz_io_issueOkInst_robAddr_3 = _zz_io_issueOkInst_robAddr_19;
      end
      2'b10 : begin
        _zz_io_issueOkInst_robAddr_3 = io_instsInfo_6_robAddr;
      end
      2'b01 : begin
        _zz_io_issueOkInst_robAddr_3 = io_instsInfo_7_robAddr;
      end
      default : begin
        _zz_io_issueOkInst_robAddr_3 = _zz_io_issueOkInst_robAddr_19;
      end
    endcase
  end

  always @(*) begin
    case(switch_IssueArbiter_l48_3)
      2'b11 : begin
        _zz_io_issueOkInst_issueQueneIdx_3 = _zz_io_issueOkInst_issueQueneIdx_11;
      end
      2'b10 : begin
        _zz_io_issueOkInst_issueQueneIdx_3 = io_instsInfo_6_issueQueneIdx;
      end
      2'b01 : begin
        _zz_io_issueOkInst_issueQueneIdx_3 = io_instsInfo_7_issueQueneIdx;
      end
      default : begin
        _zz_io_issueOkInst_issueQueneIdx_3 = _zz_io_issueOkInst_issueQueneIdx_11;
      end
    endcase
  end

  assign _zz_io_issueOkInst_ready_3 = (io_instsInfo_6_ready || io_instsInfo_7_ready);
  assign _zz_io_issueOkInst_robAddr_20 = (io_instsInfo_8_robAddr[5 : 0] < io_instsInfo_9_robAddr[5 : 0]);
  assign _zz_io_issueOkInst_robAddr_21 = ((io_instsInfo_8_robAddr[6] == io_instsInfo_9_robAddr[6]) ? _zz_io_issueOkInst_robAddr_20 : (! _zz_io_issueOkInst_robAddr_20));
  assign _zz_io_issueOkInst_robAddr_22 = (_zz_io_issueOkInst_robAddr_21 ? io_instsInfo_8_robAddr : io_instsInfo_9_robAddr);
  assign _zz_io_issueOkInst_issueQueneIdx_12 = (_zz_io_issueOkInst_robAddr_21 ? io_instsInfo_8_issueQueneIdx : io_instsInfo_9_issueQueneIdx);
  assign switch_IssueArbiter_l48_4 = {io_instsInfo_8_ready,io_instsInfo_9_ready};
  always @(*) begin
    case(switch_IssueArbiter_l48_4)
      2'b11 : begin
        _zz_io_issueOkInst_robAddr_4 = _zz_io_issueOkInst_robAddr_22;
      end
      2'b10 : begin
        _zz_io_issueOkInst_robAddr_4 = io_instsInfo_8_robAddr;
      end
      2'b01 : begin
        _zz_io_issueOkInst_robAddr_4 = io_instsInfo_9_robAddr;
      end
      default : begin
        _zz_io_issueOkInst_robAddr_4 = _zz_io_issueOkInst_robAddr_22;
      end
    endcase
  end

  always @(*) begin
    case(switch_IssueArbiter_l48_4)
      2'b11 : begin
        _zz_io_issueOkInst_issueQueneIdx_4 = _zz_io_issueOkInst_issueQueneIdx_12;
      end
      2'b10 : begin
        _zz_io_issueOkInst_issueQueneIdx_4 = io_instsInfo_8_issueQueneIdx;
      end
      2'b01 : begin
        _zz_io_issueOkInst_issueQueneIdx_4 = io_instsInfo_9_issueQueneIdx;
      end
      default : begin
        _zz_io_issueOkInst_issueQueneIdx_4 = _zz_io_issueOkInst_issueQueneIdx_12;
      end
    endcase
  end

  assign _zz_io_issueOkInst_ready_4 = (io_instsInfo_8_ready || io_instsInfo_9_ready);
  assign _zz_io_issueOkInst_robAddr_23 = (io_instsInfo_10_robAddr[5 : 0] < io_instsInfo_11_robAddr[5 : 0]);
  assign _zz_io_issueOkInst_robAddr_24 = ((io_instsInfo_10_robAddr[6] == io_instsInfo_11_robAddr[6]) ? _zz_io_issueOkInst_robAddr_23 : (! _zz_io_issueOkInst_robAddr_23));
  assign _zz_io_issueOkInst_robAddr_25 = (_zz_io_issueOkInst_robAddr_24 ? io_instsInfo_10_robAddr : io_instsInfo_11_robAddr);
  assign _zz_io_issueOkInst_issueQueneIdx_13 = (_zz_io_issueOkInst_robAddr_24 ? io_instsInfo_10_issueQueneIdx : io_instsInfo_11_issueQueneIdx);
  assign switch_IssueArbiter_l48_5 = {io_instsInfo_10_ready,io_instsInfo_11_ready};
  always @(*) begin
    case(switch_IssueArbiter_l48_5)
      2'b11 : begin
        _zz_io_issueOkInst_robAddr_5 = _zz_io_issueOkInst_robAddr_25;
      end
      2'b10 : begin
        _zz_io_issueOkInst_robAddr_5 = io_instsInfo_10_robAddr;
      end
      2'b01 : begin
        _zz_io_issueOkInst_robAddr_5 = io_instsInfo_11_robAddr;
      end
      default : begin
        _zz_io_issueOkInst_robAddr_5 = _zz_io_issueOkInst_robAddr_25;
      end
    endcase
  end

  always @(*) begin
    case(switch_IssueArbiter_l48_5)
      2'b11 : begin
        _zz_io_issueOkInst_issueQueneIdx_5 = _zz_io_issueOkInst_issueQueneIdx_13;
      end
      2'b10 : begin
        _zz_io_issueOkInst_issueQueneIdx_5 = io_instsInfo_10_issueQueneIdx;
      end
      2'b01 : begin
        _zz_io_issueOkInst_issueQueneIdx_5 = io_instsInfo_11_issueQueneIdx;
      end
      default : begin
        _zz_io_issueOkInst_issueQueneIdx_5 = _zz_io_issueOkInst_issueQueneIdx_13;
      end
    endcase
  end

  assign _zz_io_issueOkInst_ready_5 = (io_instsInfo_10_ready || io_instsInfo_11_ready);
  assign _zz_io_issueOkInst_robAddr_26 = (io_instsInfo_12_robAddr[5 : 0] < io_instsInfo_13_robAddr[5 : 0]);
  assign _zz_io_issueOkInst_robAddr_27 = ((io_instsInfo_12_robAddr[6] == io_instsInfo_13_robAddr[6]) ? _zz_io_issueOkInst_robAddr_26 : (! _zz_io_issueOkInst_robAddr_26));
  assign _zz_io_issueOkInst_robAddr_28 = (_zz_io_issueOkInst_robAddr_27 ? io_instsInfo_12_robAddr : io_instsInfo_13_robAddr);
  assign _zz_io_issueOkInst_issueQueneIdx_14 = (_zz_io_issueOkInst_robAddr_27 ? io_instsInfo_12_issueQueneIdx : io_instsInfo_13_issueQueneIdx);
  assign switch_IssueArbiter_l48_6 = {io_instsInfo_12_ready,io_instsInfo_13_ready};
  always @(*) begin
    case(switch_IssueArbiter_l48_6)
      2'b11 : begin
        _zz_io_issueOkInst_robAddr_6 = _zz_io_issueOkInst_robAddr_28;
      end
      2'b10 : begin
        _zz_io_issueOkInst_robAddr_6 = io_instsInfo_12_robAddr;
      end
      2'b01 : begin
        _zz_io_issueOkInst_robAddr_6 = io_instsInfo_13_robAddr;
      end
      default : begin
        _zz_io_issueOkInst_robAddr_6 = _zz_io_issueOkInst_robAddr_28;
      end
    endcase
  end

  always @(*) begin
    case(switch_IssueArbiter_l48_6)
      2'b11 : begin
        _zz_io_issueOkInst_issueQueneIdx_6 = _zz_io_issueOkInst_issueQueneIdx_14;
      end
      2'b10 : begin
        _zz_io_issueOkInst_issueQueneIdx_6 = io_instsInfo_12_issueQueneIdx;
      end
      2'b01 : begin
        _zz_io_issueOkInst_issueQueneIdx_6 = io_instsInfo_13_issueQueneIdx;
      end
      default : begin
        _zz_io_issueOkInst_issueQueneIdx_6 = _zz_io_issueOkInst_issueQueneIdx_14;
      end
    endcase
  end

  assign _zz_io_issueOkInst_ready_6 = (io_instsInfo_12_ready || io_instsInfo_13_ready);
  assign _zz_io_issueOkInst_robAddr_29 = (io_instsInfo_14_robAddr[5 : 0] < io_instsInfo_15_robAddr[5 : 0]);
  assign _zz_io_issueOkInst_robAddr_30 = ((io_instsInfo_14_robAddr[6] == io_instsInfo_15_robAddr[6]) ? _zz_io_issueOkInst_robAddr_29 : (! _zz_io_issueOkInst_robAddr_29));
  assign _zz_io_issueOkInst_robAddr_31 = (_zz_io_issueOkInst_robAddr_30 ? io_instsInfo_14_robAddr : io_instsInfo_15_robAddr);
  assign _zz_io_issueOkInst_issueQueneIdx_15 = (_zz_io_issueOkInst_robAddr_30 ? io_instsInfo_14_issueQueneIdx : io_instsInfo_15_issueQueneIdx);
  assign switch_IssueArbiter_l48_7 = {io_instsInfo_14_ready,io_instsInfo_15_ready};
  always @(*) begin
    case(switch_IssueArbiter_l48_7)
      2'b11 : begin
        _zz_io_issueOkInst_robAddr_7 = _zz_io_issueOkInst_robAddr_31;
      end
      2'b10 : begin
        _zz_io_issueOkInst_robAddr_7 = io_instsInfo_14_robAddr;
      end
      2'b01 : begin
        _zz_io_issueOkInst_robAddr_7 = io_instsInfo_15_robAddr;
      end
      default : begin
        _zz_io_issueOkInst_robAddr_7 = _zz_io_issueOkInst_robAddr_31;
      end
    endcase
  end

  always @(*) begin
    case(switch_IssueArbiter_l48_7)
      2'b11 : begin
        _zz_io_issueOkInst_issueQueneIdx_7 = _zz_io_issueOkInst_issueQueneIdx_15;
      end
      2'b10 : begin
        _zz_io_issueOkInst_issueQueneIdx_7 = io_instsInfo_14_issueQueneIdx;
      end
      2'b01 : begin
        _zz_io_issueOkInst_issueQueneIdx_7 = io_instsInfo_15_issueQueneIdx;
      end
      default : begin
        _zz_io_issueOkInst_issueQueneIdx_7 = _zz_io_issueOkInst_issueQueneIdx_15;
      end
    endcase
  end

  assign _zz_io_issueOkInst_ready_7 = (io_instsInfo_14_ready || io_instsInfo_15_ready);
  assign _zz_io_issueOkInst_robAddr_36 = (_zz_io_issueOkInst_robAddr[5 : 0] < _zz_io_issueOkInst_robAddr_1[5 : 0]);
  assign _zz_io_issueOkInst_robAddr_37 = ((_zz_io_issueOkInst_robAddr[6] == _zz_io_issueOkInst_robAddr_1[6]) ? _zz_io_issueOkInst_robAddr_36 : (! _zz_io_issueOkInst_robAddr_36));
  assign _zz_io_issueOkInst_robAddr_38 = (_zz_io_issueOkInst_robAddr_37 ? _zz_io_issueOkInst_robAddr : _zz_io_issueOkInst_robAddr_1);
  assign _zz_io_issueOkInst_issueQueneIdx_20 = (_zz_io_issueOkInst_robAddr_37 ? _zz_io_issueOkInst_issueQueneIdx : _zz_io_issueOkInst_issueQueneIdx_1);
  assign switch_IssueArbiter_l48_8 = {_zz_io_issueOkInst_ready,_zz_io_issueOkInst_ready_1};
  always @(*) begin
    case(switch_IssueArbiter_l48_8)
      2'b11 : begin
        _zz_io_issueOkInst_robAddr_32 = _zz_io_issueOkInst_robAddr_38;
      end
      2'b10 : begin
        _zz_io_issueOkInst_robAddr_32 = _zz_io_issueOkInst_robAddr;
      end
      2'b01 : begin
        _zz_io_issueOkInst_robAddr_32 = _zz_io_issueOkInst_robAddr_1;
      end
      default : begin
        _zz_io_issueOkInst_robAddr_32 = _zz_io_issueOkInst_robAddr_38;
      end
    endcase
  end

  always @(*) begin
    case(switch_IssueArbiter_l48_8)
      2'b11 : begin
        _zz_io_issueOkInst_issueQueneIdx_16 = _zz_io_issueOkInst_issueQueneIdx_20;
      end
      2'b10 : begin
        _zz_io_issueOkInst_issueQueneIdx_16 = _zz_io_issueOkInst_issueQueneIdx;
      end
      2'b01 : begin
        _zz_io_issueOkInst_issueQueneIdx_16 = _zz_io_issueOkInst_issueQueneIdx_1;
      end
      default : begin
        _zz_io_issueOkInst_issueQueneIdx_16 = _zz_io_issueOkInst_issueQueneIdx_20;
      end
    endcase
  end

  assign _zz_io_issueOkInst_ready_8 = (_zz_io_issueOkInst_ready || _zz_io_issueOkInst_ready_1);
  assign _zz_io_issueOkInst_robAddr_39 = (_zz_io_issueOkInst_robAddr_2[5 : 0] < _zz_io_issueOkInst_robAddr_3[5 : 0]);
  assign _zz_io_issueOkInst_robAddr_40 = ((_zz_io_issueOkInst_robAddr_2[6] == _zz_io_issueOkInst_robAddr_3[6]) ? _zz_io_issueOkInst_robAddr_39 : (! _zz_io_issueOkInst_robAddr_39));
  assign _zz_io_issueOkInst_robAddr_41 = (_zz_io_issueOkInst_robAddr_40 ? _zz_io_issueOkInst_robAddr_2 : _zz_io_issueOkInst_robAddr_3);
  assign _zz_io_issueOkInst_issueQueneIdx_21 = (_zz_io_issueOkInst_robAddr_40 ? _zz_io_issueOkInst_issueQueneIdx_2 : _zz_io_issueOkInst_issueQueneIdx_3);
  assign switch_IssueArbiter_l48_9 = {_zz_io_issueOkInst_ready_2,_zz_io_issueOkInst_ready_3};
  always @(*) begin
    case(switch_IssueArbiter_l48_9)
      2'b11 : begin
        _zz_io_issueOkInst_robAddr_33 = _zz_io_issueOkInst_robAddr_41;
      end
      2'b10 : begin
        _zz_io_issueOkInst_robAddr_33 = _zz_io_issueOkInst_robAddr_2;
      end
      2'b01 : begin
        _zz_io_issueOkInst_robAddr_33 = _zz_io_issueOkInst_robAddr_3;
      end
      default : begin
        _zz_io_issueOkInst_robAddr_33 = _zz_io_issueOkInst_robAddr_41;
      end
    endcase
  end

  always @(*) begin
    case(switch_IssueArbiter_l48_9)
      2'b11 : begin
        _zz_io_issueOkInst_issueQueneIdx_17 = _zz_io_issueOkInst_issueQueneIdx_21;
      end
      2'b10 : begin
        _zz_io_issueOkInst_issueQueneIdx_17 = _zz_io_issueOkInst_issueQueneIdx_2;
      end
      2'b01 : begin
        _zz_io_issueOkInst_issueQueneIdx_17 = _zz_io_issueOkInst_issueQueneIdx_3;
      end
      default : begin
        _zz_io_issueOkInst_issueQueneIdx_17 = _zz_io_issueOkInst_issueQueneIdx_21;
      end
    endcase
  end

  assign _zz_io_issueOkInst_ready_9 = (_zz_io_issueOkInst_ready_2 || _zz_io_issueOkInst_ready_3);
  assign _zz_io_issueOkInst_robAddr_42 = (_zz_io_issueOkInst_robAddr_4[5 : 0] < _zz_io_issueOkInst_robAddr_5[5 : 0]);
  assign _zz_io_issueOkInst_robAddr_43 = ((_zz_io_issueOkInst_robAddr_4[6] == _zz_io_issueOkInst_robAddr_5[6]) ? _zz_io_issueOkInst_robAddr_42 : (! _zz_io_issueOkInst_robAddr_42));
  assign _zz_io_issueOkInst_robAddr_44 = (_zz_io_issueOkInst_robAddr_43 ? _zz_io_issueOkInst_robAddr_4 : _zz_io_issueOkInst_robAddr_5);
  assign _zz_io_issueOkInst_issueQueneIdx_22 = (_zz_io_issueOkInst_robAddr_43 ? _zz_io_issueOkInst_issueQueneIdx_4 : _zz_io_issueOkInst_issueQueneIdx_5);
  assign switch_IssueArbiter_l48_10 = {_zz_io_issueOkInst_ready_4,_zz_io_issueOkInst_ready_5};
  always @(*) begin
    case(switch_IssueArbiter_l48_10)
      2'b11 : begin
        _zz_io_issueOkInst_robAddr_34 = _zz_io_issueOkInst_robAddr_44;
      end
      2'b10 : begin
        _zz_io_issueOkInst_robAddr_34 = _zz_io_issueOkInst_robAddr_4;
      end
      2'b01 : begin
        _zz_io_issueOkInst_robAddr_34 = _zz_io_issueOkInst_robAddr_5;
      end
      default : begin
        _zz_io_issueOkInst_robAddr_34 = _zz_io_issueOkInst_robAddr_44;
      end
    endcase
  end

  always @(*) begin
    case(switch_IssueArbiter_l48_10)
      2'b11 : begin
        _zz_io_issueOkInst_issueQueneIdx_18 = _zz_io_issueOkInst_issueQueneIdx_22;
      end
      2'b10 : begin
        _zz_io_issueOkInst_issueQueneIdx_18 = _zz_io_issueOkInst_issueQueneIdx_4;
      end
      2'b01 : begin
        _zz_io_issueOkInst_issueQueneIdx_18 = _zz_io_issueOkInst_issueQueneIdx_5;
      end
      default : begin
        _zz_io_issueOkInst_issueQueneIdx_18 = _zz_io_issueOkInst_issueQueneIdx_22;
      end
    endcase
  end

  assign _zz_io_issueOkInst_ready_10 = (_zz_io_issueOkInst_ready_4 || _zz_io_issueOkInst_ready_5);
  assign _zz_io_issueOkInst_robAddr_45 = (_zz_io_issueOkInst_robAddr_6[5 : 0] < _zz_io_issueOkInst_robAddr_7[5 : 0]);
  assign _zz_io_issueOkInst_robAddr_46 = ((_zz_io_issueOkInst_robAddr_6[6] == _zz_io_issueOkInst_robAddr_7[6]) ? _zz_io_issueOkInst_robAddr_45 : (! _zz_io_issueOkInst_robAddr_45));
  assign _zz_io_issueOkInst_robAddr_47 = (_zz_io_issueOkInst_robAddr_46 ? _zz_io_issueOkInst_robAddr_6 : _zz_io_issueOkInst_robAddr_7);
  assign _zz_io_issueOkInst_issueQueneIdx_23 = (_zz_io_issueOkInst_robAddr_46 ? _zz_io_issueOkInst_issueQueneIdx_6 : _zz_io_issueOkInst_issueQueneIdx_7);
  assign switch_IssueArbiter_l48_11 = {_zz_io_issueOkInst_ready_6,_zz_io_issueOkInst_ready_7};
  always @(*) begin
    case(switch_IssueArbiter_l48_11)
      2'b11 : begin
        _zz_io_issueOkInst_robAddr_35 = _zz_io_issueOkInst_robAddr_47;
      end
      2'b10 : begin
        _zz_io_issueOkInst_robAddr_35 = _zz_io_issueOkInst_robAddr_6;
      end
      2'b01 : begin
        _zz_io_issueOkInst_robAddr_35 = _zz_io_issueOkInst_robAddr_7;
      end
      default : begin
        _zz_io_issueOkInst_robAddr_35 = _zz_io_issueOkInst_robAddr_47;
      end
    endcase
  end

  always @(*) begin
    case(switch_IssueArbiter_l48_11)
      2'b11 : begin
        _zz_io_issueOkInst_issueQueneIdx_19 = _zz_io_issueOkInst_issueQueneIdx_23;
      end
      2'b10 : begin
        _zz_io_issueOkInst_issueQueneIdx_19 = _zz_io_issueOkInst_issueQueneIdx_6;
      end
      2'b01 : begin
        _zz_io_issueOkInst_issueQueneIdx_19 = _zz_io_issueOkInst_issueQueneIdx_7;
      end
      default : begin
        _zz_io_issueOkInst_issueQueneIdx_19 = _zz_io_issueOkInst_issueQueneIdx_23;
      end
    endcase
  end

  assign _zz_io_issueOkInst_ready_11 = (_zz_io_issueOkInst_ready_6 || _zz_io_issueOkInst_ready_7);
  assign _zz_io_issueOkInst_robAddr_50 = (_zz_io_issueOkInst_robAddr_32[5 : 0] < _zz_io_issueOkInst_robAddr_33[5 : 0]);
  assign _zz_io_issueOkInst_robAddr_51 = ((_zz_io_issueOkInst_robAddr_32[6] == _zz_io_issueOkInst_robAddr_33[6]) ? _zz_io_issueOkInst_robAddr_50 : (! _zz_io_issueOkInst_robAddr_50));
  assign _zz_io_issueOkInst_robAddr_52 = (_zz_io_issueOkInst_robAddr_51 ? _zz_io_issueOkInst_robAddr_32 : _zz_io_issueOkInst_robAddr_33);
  assign _zz_io_issueOkInst_issueQueneIdx_26 = (_zz_io_issueOkInst_robAddr_51 ? _zz_io_issueOkInst_issueQueneIdx_16 : _zz_io_issueOkInst_issueQueneIdx_17);
  assign switch_IssueArbiter_l48_12 = {_zz_io_issueOkInst_ready_8,_zz_io_issueOkInst_ready_9};
  always @(*) begin
    case(switch_IssueArbiter_l48_12)
      2'b11 : begin
        _zz_io_issueOkInst_robAddr_48 = _zz_io_issueOkInst_robAddr_52;
      end
      2'b10 : begin
        _zz_io_issueOkInst_robAddr_48 = _zz_io_issueOkInst_robAddr_32;
      end
      2'b01 : begin
        _zz_io_issueOkInst_robAddr_48 = _zz_io_issueOkInst_robAddr_33;
      end
      default : begin
        _zz_io_issueOkInst_robAddr_48 = _zz_io_issueOkInst_robAddr_52;
      end
    endcase
  end

  always @(*) begin
    case(switch_IssueArbiter_l48_12)
      2'b11 : begin
        _zz_io_issueOkInst_issueQueneIdx_24 = _zz_io_issueOkInst_issueQueneIdx_26;
      end
      2'b10 : begin
        _zz_io_issueOkInst_issueQueneIdx_24 = _zz_io_issueOkInst_issueQueneIdx_16;
      end
      2'b01 : begin
        _zz_io_issueOkInst_issueQueneIdx_24 = _zz_io_issueOkInst_issueQueneIdx_17;
      end
      default : begin
        _zz_io_issueOkInst_issueQueneIdx_24 = _zz_io_issueOkInst_issueQueneIdx_26;
      end
    endcase
  end

  assign _zz_io_issueOkInst_ready_12 = (_zz_io_issueOkInst_ready_8 || _zz_io_issueOkInst_ready_9);
  assign _zz_io_issueOkInst_robAddr_53 = (_zz_io_issueOkInst_robAddr_34[5 : 0] < _zz_io_issueOkInst_robAddr_35[5 : 0]);
  assign _zz_io_issueOkInst_robAddr_54 = ((_zz_io_issueOkInst_robAddr_34[6] == _zz_io_issueOkInst_robAddr_35[6]) ? _zz_io_issueOkInst_robAddr_53 : (! _zz_io_issueOkInst_robAddr_53));
  assign _zz_io_issueOkInst_robAddr_55 = (_zz_io_issueOkInst_robAddr_54 ? _zz_io_issueOkInst_robAddr_34 : _zz_io_issueOkInst_robAddr_35);
  assign _zz_io_issueOkInst_issueQueneIdx_27 = (_zz_io_issueOkInst_robAddr_54 ? _zz_io_issueOkInst_issueQueneIdx_18 : _zz_io_issueOkInst_issueQueneIdx_19);
  assign switch_IssueArbiter_l48_13 = {_zz_io_issueOkInst_ready_10,_zz_io_issueOkInst_ready_11};
  always @(*) begin
    case(switch_IssueArbiter_l48_13)
      2'b11 : begin
        _zz_io_issueOkInst_robAddr_49 = _zz_io_issueOkInst_robAddr_55;
      end
      2'b10 : begin
        _zz_io_issueOkInst_robAddr_49 = _zz_io_issueOkInst_robAddr_34;
      end
      2'b01 : begin
        _zz_io_issueOkInst_robAddr_49 = _zz_io_issueOkInst_robAddr_35;
      end
      default : begin
        _zz_io_issueOkInst_robAddr_49 = _zz_io_issueOkInst_robAddr_55;
      end
    endcase
  end

  always @(*) begin
    case(switch_IssueArbiter_l48_13)
      2'b11 : begin
        _zz_io_issueOkInst_issueQueneIdx_25 = _zz_io_issueOkInst_issueQueneIdx_27;
      end
      2'b10 : begin
        _zz_io_issueOkInst_issueQueneIdx_25 = _zz_io_issueOkInst_issueQueneIdx_18;
      end
      2'b01 : begin
        _zz_io_issueOkInst_issueQueneIdx_25 = _zz_io_issueOkInst_issueQueneIdx_19;
      end
      default : begin
        _zz_io_issueOkInst_issueQueneIdx_25 = _zz_io_issueOkInst_issueQueneIdx_27;
      end
    endcase
  end

  assign _zz_io_issueOkInst_ready_13 = (_zz_io_issueOkInst_ready_10 || _zz_io_issueOkInst_ready_11);
  assign _zz_io_issueOkInst_robAddr_57 = (_zz_io_issueOkInst_robAddr_48[5 : 0] < _zz_io_issueOkInst_robAddr_49[5 : 0]);
  assign _zz_io_issueOkInst_robAddr_58 = ((_zz_io_issueOkInst_robAddr_48[6] == _zz_io_issueOkInst_robAddr_49[6]) ? _zz_io_issueOkInst_robAddr_57 : (! _zz_io_issueOkInst_robAddr_57));
  assign _zz_io_issueOkInst_robAddr_59 = (_zz_io_issueOkInst_robAddr_58 ? _zz_io_issueOkInst_robAddr_48 : _zz_io_issueOkInst_robAddr_49);
  assign _zz_io_issueOkInst_issueQueneIdx_29 = (_zz_io_issueOkInst_robAddr_58 ? _zz_io_issueOkInst_issueQueneIdx_24 : _zz_io_issueOkInst_issueQueneIdx_25);
  assign switch_IssueArbiter_l48_14 = {_zz_io_issueOkInst_ready_12,_zz_io_issueOkInst_ready_13};
  always @(*) begin
    case(switch_IssueArbiter_l48_14)
      2'b11 : begin
        _zz_io_issueOkInst_robAddr_56 = _zz_io_issueOkInst_robAddr_59;
      end
      2'b10 : begin
        _zz_io_issueOkInst_robAddr_56 = _zz_io_issueOkInst_robAddr_48;
      end
      2'b01 : begin
        _zz_io_issueOkInst_robAddr_56 = _zz_io_issueOkInst_robAddr_49;
      end
      default : begin
        _zz_io_issueOkInst_robAddr_56 = _zz_io_issueOkInst_robAddr_59;
      end
    endcase
  end

  always @(*) begin
    case(switch_IssueArbiter_l48_14)
      2'b11 : begin
        _zz_io_issueOkInst_issueQueneIdx_28 = _zz_io_issueOkInst_issueQueneIdx_29;
      end
      2'b10 : begin
        _zz_io_issueOkInst_issueQueneIdx_28 = _zz_io_issueOkInst_issueQueneIdx_24;
      end
      2'b01 : begin
        _zz_io_issueOkInst_issueQueneIdx_28 = _zz_io_issueOkInst_issueQueneIdx_25;
      end
      default : begin
        _zz_io_issueOkInst_issueQueneIdx_28 = _zz_io_issueOkInst_issueQueneIdx_29;
      end
    endcase
  end

  always @(posedge clk) begin
    io_issueOkInst_robAddr <= _zz_io_issueOkInst_robAddr_56;
    io_issueOkInst_issueQueneIdx <= _zz_io_issueOkInst_issueQueneIdx_28;
    io_issueOkInst_ready <= (_zz_io_issueOkInst_ready_12 || _zz_io_issueOkInst_ready_13);
  end


endmodule
