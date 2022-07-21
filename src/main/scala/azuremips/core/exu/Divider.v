module Divider (
    input         clk,
    input         reset,
    input         valid,
    input  [31:0] a,
    input  [31:0] b,
    output        done,
    output [63:0] res
);

    localparam INIT  = 1'b0;
    localparam DOING = 1'b1;
    localparam DIV_DELAY = {2'b0, 1'b1, 32'b0};

    reg        state, state_nxt;
    reg [34:0] count, count_nxt;
    reg [63:0] prod, prod_nxt;

    always @(posedge clk) begin
        if (reset) begin
            {state, count} <= 36'b0;
        end else begin
            {state, count} <= {state_nxt, count_nxt};
        end
    end
    assign done = (state_nxt == INIT);
    always @(*) begin
        case(state)
            INIT: begin
                if (valid) begin
                    state_nxt = DOING;
                    count_nxt = DIV_DELAY;
                end else begin
                    state_nxt = state;
                    count_nxt = count;
                end
            end
            DOING: begin
                count_nxt = {1'b0, count_nxt[34:1]};
                if (count_nxt == 35'b0) begin
                    state_nxt = INIT;
                end else begin
                    state_nxt = state;
                end
            end
            default: begin
                state_nxt = state;
                count_nxt = count;
            end
        endcase
    end
    always @(*) begin
        case(state)
            INIT: begin
                prod_nxt = {32'b0, a};
            end
            DOING: begin
                if (prod_nxt[62:31] >= b) begin
                    prod_nxt = {(prod_nxt[62:31] - b), prod_nxt[30:0], 1'b1};
                end else begin
                    prod_nxt = {prod_nxt[62:0], 1'b0};
                end
            end
            default: begin
                prod_nxt = prod;
            end
        endcase
    end
    always @(posedge clk) begin
        if (reset) begin
            prod <= 64'b0;
        end else begin
            prod <= prod_nxt;
        end
    end
    assign res = prod;
endmodule