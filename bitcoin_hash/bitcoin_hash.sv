module bitcoin_hash (input logic        clk, reset_n, start,
                     input logic [15:0] message_addr, output_addr,
                    output logic        done, mem_clk, mem_we,
                    output logic [15:0] mem_addr,
                    output logic [31:0] mem_write_data,
                     input logic [31:0] mem_read_data);

parameter num_nonces = 16;
parameter num_words = 21;
// logic [ 4:0] state;
// logic [31:0] hout[num_nonces];

parameter int k[64] = '{
    32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
    32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
    32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
    32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
    32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
    32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
    32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
    32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};

// Student to add rest of the code here
enum logic [3:0] {IDLE, LOAD, PHASE_1, PHASE_2, PHASE_3, WAIT_1, WAIT_2, WRITE} state;
logic [31:0] w[16][16];
logic [31:0] message[32];
logic [31:0] ha[8];
logic [31:0] hb[16][8];
logic [7:0] offset;
logic cur_we;
logic [15:0] cur_addr;
logic [31:0] cur_write_data;
logic start_a, start_b, in_h;
logic finish_a;
logic finish_b[16];

 assign mem_clk = clk;
 assign mem_addr = cur_addr + offset;
 assign mem_we = cur_we;
 assign mem_write_data = cur_write_data;

always_ff @(posedge clk, negedge reset_n) begin
    if(!reset_n) begin
    cur_we <= 1'b0;
    state <= IDLE;
    end
    else case (state)
    IDLE:begin // Init
        if(start) begin
        offset <= 8'b0;
        cur_we <= 1'b0;
        cur_addr <= message_addr;
        start_a <= 0;
        start_b <= 0;
        state <= LOAD;
        end
    end
    LOAD: begin 
        state <= PHASE_1;
    end
    PHASE_1: begin
    if(offset <= num_words) begin
        message[offset] <= mem_read_data;
        offset <= offset + 8'b1;
        state <= LOAD;
    end 
    else begin
	     for(int n = 0; n < num_nonces; n++) w[0][n] <= message[n];
		  message[20] <= 32'h80000000;
		  for(int n = 21; n < 31; n++) message[n] <= 32'b0;
		  message[31] <= 32'd640;
		  in_h <= 1;
		  start_a <= 1;
		  state <= PHASE_2;
		end
    end
    PHASE_2: begin
    start_a <=0;
    if(finish_a) begin
     for(int i = 0; i < num_nonces; i++) begin
        for (int n = 0; n < 3; n++) w[i][n] <= message[n + 16];
		    w[i][3] <= i;
		for (int n = 4; n < 16; n++) w[i][n] <= message[n + 16];
     end
     in_h <= 0 ;
     start_b <= 1;
     state <=PHASE_3;
    end
        else begin
        state <= PHASE_2;
        end
    end
    PHASE_3: begin
        start_b <=0;
        if(finish_b[0]) begin
        for(int i = 0; i< num_nonces; i++)begin
            for (int n = 0; n < 8; n++) w[i][n] <= hb[i][n];;
			 w[i][8] <= 32'h80000000;
			 for (int n = 9; n < 15; n++) w[i][n] <= 32'b0; 
			 w[i][15] <= 32'd256;
        end
        state <= WAIT_1;
        in_h <= 1;
        start_b <=1;

        end
        else state <= PHASE_3;
    end
    WAIT_1: begin
        state <= WAIT_2;
    end

    WAIT_2: begin
        start_b <= 0;
        if(finish_b[0]) begin
        offset <=8'b0;
        cur_we <= 1'b1;
        cur_addr <= output_addr;
        cur_write_data <= hb[0][0];
        state <= WRITE;
        end

        else begin
         state <= WAIT_2;
        end
    end
     WRITE: begin
	   if(offset < 15) begin
		  case(offset)
		      0: cur_write_data <= hb[ 1][0];
			  1: cur_write_data <= hb[ 2][0];
			  2: cur_write_data <= hb[ 3][0];
			  3: cur_write_data <= hb[ 4][0];
			  4: cur_write_data <= hb[ 5][0];
			  5: cur_write_data <= hb[ 6][0];
			  6: cur_write_data <= hb[ 7][0];
			  7: cur_write_data <= hb[ 8][0];
			  8: cur_write_data <= hb[ 9][0];
			  9: cur_write_data <= hb[10][0];
			 10: cur_write_data <= hb[11][0];
			 11: cur_write_data <= hb[12][0];
			 12: cur_write_data <= hb[13][0];
			 13: cur_write_data <= hb[14][0];
			 14: cur_write_data <= hb[15][0];
			 default: cur_write_data <= hb[1][0];
		  endcase
		  offset <= offset + 8'b1;
		  state <= WRITE;
		end
		else state <= IDLE;
    end
    endcase
  end
simplified_sha256 insha(
  .clk(clk),
  .reset_n(reset_n),
  .start(start_a),
  .message(w[0]),
  .hash(ha),
  .k(k),
  .oh(in_h),
  .done(finish_a),
  .result(ha)
);
genvar i;
generate
    for(i = 0; i<16;i++) begin: gblock
    simplified_sha256 gsha (
	   .clk(clk),
      .reset_n(reset_n),
      .start(start_b),
      .message(w[i]),
      .hash(ha),
      .k(k),
      .oh(in_h),
      .done(finish_b[i]),
      .result(hb[i])
    );
    end
endgenerate
assign done = (state == IDLE);
endmodule
