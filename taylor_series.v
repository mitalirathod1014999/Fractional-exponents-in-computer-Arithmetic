// Code your design here
module exp(
  input [31:0] x,
  output reg [31:0] out
);
  
  wire [31:0] mem [10]; // store fractional values
  wire [31:0] sum [10];
  wire [31:0] curr_mult [10];
  wire [31:0] curr_taylor [10];
  
  parameter one_val = 32'b00111111100000000000000000000000; // 1.0 in IEEE 754 format
  
  // Initialize mem[] with values of 1/n! for n = 1 to 10
 
  assign mem[0] = 32'b00111111100000000000000000000000; // 1/1!
  assign mem[1] = 32'b00111111000000000000000000000000; // 1/2!
  assign mem[2] = 32'b00111110001010101010101010101011; // 1/3!
  assign mem[3] = 32'b00111101001010101010101010101011; // 1/4!
  assign mem[4] = 32'b00111100000010001000100010001001; // 1/5!
  assign mem[5] = 32'b00111010101101100000101101100001; // 1/6!
  assign mem[6] = 32'b00111001010100000000110100000001; // 1/7!
  assign mem[7] = 32'b00110111110100000000110100000001; // 1/8!
  assign mem[8] = 32'b00110110001110001110111100011101; // 1/9!
  assign mem[9] = 32'b00110100100100111111001001111110; // 1/10!
  
  Multiplication inst(one_val,x,curr_mult[0]);
  Multiplication inst1(mem[0],curr_mult[0],curr_taylor[0]);
  FloatingPointAdder int3(one_val,curr_taylor[0],sum[0]);
  
  genvar g;
  
  generate 
    for(g=1;g<10;g=g+1) begin
      Multiplication inst4(curr_mult[g-1], x, curr_mult[g]); // curr_mult * x
      Multiplication inst5(mem[g], curr_mult[g], curr_taylor[g]); // mem[g] * 1/n!
      FloatingPointAdder inst6(sum[g-1], curr_taylor[g], sum[g]); // new_sum = sum + curr_taylor
    end
  endgenerate
  
  assign out = sum[9];
  
endmodule

module  FloatingPointAdder(  input [31:0] A,
input [31:0] B, output [31:0] Out
);
wire [22:0]MA; wire [22:0]MB; wire [7:0]EA; wire [7:0]EB;

assign MA[22:0]=A[22:0]; 
assign MB[22:0]=B[22:0];

assign EB = B[30:23];
assign EA = A[30:23];
  
wire [7:0] Modulo;
wire Borrow;
  
Sub_Result subtract(EA,EB,Modulo,Borrow);
  
wire [23:0]mux1out;
wire [23:0]mux2out;
  
Mux24 rightshiftertop(MB,MA,Borrow,mux1out);
Mux24 addertop(MA,MB,Borrow,mux2out);
wire [23:0]outshift;

wire [4:0]shiftdiff;
assign shiftdiff = Modulo[4:0];
  
BarrelShifter rightshift(mux1out,outshift,shiftdiff);
  
wire [23:0]adderout;
wire cout;
Adder_24Bit A1(mux2out,outshift,adderout,cout);
  
wire [7:0]maxexp ;
Mux_8 expmax(EA,EB,Borrow,maxexp);
  
wire [7:0]expfinal;
ControlledIncrementor finexp(cout,maxexp,expfinal);
assign Out[30:23] = expfinal;  
  
wire [23:0]finalM;
wire [4:0]select;
assign select[4:1] = 4'b0000;
assign select[0] = cout;
BarrelShifter finalshifter(adderout,finalM,select); 
assign Out[31] = 0;//Positive Number Addition
assign Out[22:0] = finalM;  


endmodule 
//----------------------------------------------------------------//
module Adder_24Bit(
    input [23:0] A,
    input [23:0] B,
    output [23:0] Out,
    output Cout
    );
	 
 wire [23:0] Cin;
 HalfAdder H1(A[0],B[0],Out[0],Cin[0]);
 
 genvar j;
 generate 
   for(j=1;j<=23;j=j+1)
    begin
     FullAdder F1(A[j],B[j],Cin[j-1],Out[j],Cin[j]);
    end
 assign Cout = Cin[23];
endgenerate

endmodule  
  
//----------------------------------------------------

module BarrelShifter(
    input [23:0] In,
    output [23:0] Out,
    input [4:0] Shift
    );
	 
 wire [23:0]a;
 genvar i;
 
 generate
		for(i=0;i<23;i=i+1)
			begin
			Mux M(In[i] , In[i+1] , Shift[0] , a[i]);
			end
	 Mux M1(In[23] , 1'b0 , Shift[0] , a[23]);
endgenerate

wire [23:0]a1;
genvar j , k;

generate
		for(j=0;j<22;j=j+1)
			begin
		    	Mux M2(a[j] , a[j+2] , Shift[1] , a1[j]);
			end
	    for(k=22;k<24;k=k+1)
			begin
			   Mux M3(a[k] , 1'b0 , Shift[1] , a1[k]);
            end
endgenerate

genvar p , q;
wire [23:0]a2;

generate
		for(p=0;p<20;p=p+1)
			begin
			 Mux M4(a1[p] , a1[p+4] , Shift[2] , a2[p]);
			end
		for(k=20;k<24;k=k+1)
			begin
			 Mux M5(a1[k] , 1'b0 , Shift[2] , a2[k]);
			end
endgenerate

genvar x , y;
wire [23:0]a3;

generate
		for(x=0;x<16;x=x+1)
		 begin
		  Mux M6(a2[x] , a2[x+8] , Shift[3] , a3[x]);
		 end
		for(y=16;y<24;y=y+1)
		 begin
		  Mux M7(a2[y] , 1'b0 , Shift[3] , a3[y]);
		 end
endgenerate

genvar s , t;
wire [23:0]a4;

generate
	for(s=0;s<8;s=s+1)
		begin
		 Mux M8(a3[s] , a3[s+4] , Shift[4] , a4[s]);
		end
	for(t=8;t<24;t=t+1)
		begin
		 Mux M9(a3[t] , 1'b0 , Shift[4] , a4[t]);
		end
endgenerate

assign Out = a4;

endmodule
  
//--------------------------------------------------------------//
  
module ControlledIncrementor(
    input A,
    input [7:0] Z,
    output [7:0] Out
    );
	 
wire [7:0]w;
wire [7:0]Cin;

assign w =(A==1'b1)?1:0;

HalfAdder H(Z[0],w[0],Out[0],Cin[0]);

genvar j;
generate 
  for(j=1;j<8;j=j+1)
   begin
   FullAdder F1(Z[j],w[j],Cin[j-1],Out[j],Cin[j]);
   end
endgenerate

endmodule  
  
//-----------------------------------------------
  
module FullAdder(
    input A,
    input B,
    input Cin,
    output S,
    output Cout
    );
	 
wire w1,w2,w3;
xor(w1,A,B);
xor(S,w1,Cin);
and(w2,w1,Cin);
and(w3,A,B);
or(Cout,w3,w2);

endmodule

  
  //---------------------------
  
module FullSubtractor(
    input A,
    input B,
    input Bin,
    output D,
    output Bout
    );
	 
wire w1,w2,w3,w4,w5;

xor(w1,A,B);
xor(D,w1,Bin);

and(w2,~A,~B,Bin);
and(w3,A,~B,~Bin);
and(w4,~A,B,~Bin);
and(w5,A,B,Bin);

or(Bout,w2,w3,w4,w5);

endmodule
  
/////////////////-------------------------
module HalfAdder(
    input A,
    input B,
    output S,
    output C
    );
	 
xor(S,A,B);
and(C,A,B);

endmodule
  
//////////------------------------
  
module HalfSubtractor(
    input A,
    input B,
    output D,
    output Bout
    );
	 
xor(D,A,B);
and(Bout,~A,B);

endmodule
  
/////////////////////////////////////////
  
module Mux(
    input In0,
    input In1,
    input S,
	 output Out
    );
	 
wire w1,w2; 
 
and(w1,~S,In0);
and(w2,S,In1);
or (Out,w1,w2);	 

endmodule

//------------------------------------
  
module Mux24(

    input [22:0] A,
    input [22:0] B,
    input S,
	 output [23:0] Out
    );
	 
genvar i;

generate
  for(i=0;i<23;i = i + 1) begin
    Mux M(A[i],B[i],S,Out[i]);
  end
endgenerate

assign Out[23]=1'b1;

endmodule

//------------------------------------------
  
module Mux_8(
    input [7:0] A,
    input [7:0] B,
    input C,
    output [7:0] Out
    );
	 
 genvar j;
 
 generate 
   for(j=0;j<=7;j=j+1)
    begin
     Mux M(A[j],B[j],C,Out[j]);
    end
endgenerate

endmodule
  
//--------------------------------------------------------
  
 module Sub_Result(
    input [7:0] A,
    input [7:0] B,
    output [7:0] Out,
	output b
);
	 
wire [7:0]d;
wire [7:0]d1;

Subtractor_8Bit S(A,B,d,b);
Complement2s C(d, d1);
   
assign Out= (b == 1'b1) ? d1:d;
   
endmodule
  
//---------------------------------------------------------
  
module Subtractor_8Bit(

    input [7:0] A,
    input [7:0] B,
    output [7:0] D,
    output Bout
    );
	 
wire [7:0] w;

HalfSubtractor H1(A[0],B[0],D[0],w[0]);
FullSubtractor F1(A[1],B[1],w[0],D[1],w[1]);
FullSubtractor F2(A[2],B[2],w[1],D[2],w[2]);
FullSubtractor F3(A[3],B[3],w[2],D[3],w[3]);
FullSubtractor F4(A[4],B[4],w[3],D[4],w[4]);
FullSubtractor F5(A[5],B[5],w[4],D[5],w[5]);
FullSubtractor F6(A[6],B[6],w[5],D[6],w[6]);
FullSubtractor F7(A[7],B[7],w[6],D[7],w[7]);

assign Bout=w[7];

endmodule
  
//--------------------------  

module Complement2s(
    input [7:0] A,
    output [7:0] Out
    );
	 
wire [7:0]w;
genvar j;

 generate
   for(j=0;j<8;j=j+1)
    begin
     Mux M(1'b1,1'b0,A[j],w[j]);
    end
endgenerate

wire [7:0]c;
HalfAdder H1(w[0],1'b1,Out[0],c[0]);

genvar i;
	generate
			for(i=1;i<8;i=i+1)
			  begin
			    FullAdder F1(w[i],1'b0,c[i-1],Out[i],c[i]);
			  end
	endgenerate
	
endmodule
  

//---------------------------------------------------------------------------------------

module Multiplication(
		input [31:0] a_operand,
		input [31:0] b_operand,
		output [31:0] result
		);

wire sign,product_round,normalised,zero;
wire [8:0] exponent,sum_exponent;
wire [22:0] product_mantissa;
wire [23:0] operand_a,operand_b;
wire [47:0] product,product_normalised; //48 Bits
wire Exception,Overflow,Underflow;


assign sign = a_operand[31] ^ b_operand[31];

//Exception flag sets 1 if either one of the exponent is 255.
assign Exception = (&a_operand[30:23]) | (&b_operand[30:23]);

//Assigining significand values according to Hidden Bit.
//If exponent is equal to zero then hidden bit will be 0 for that respective significand else it will be 1

assign operand_a = (|a_operand[30:23]) ? {1'b1,a_operand[22:0]} : {1'b0,a_operand[22:0]};

assign operand_b = (|b_operand[30:23]) ? {1'b1,b_operand[22:0]} : {1'b0,b_operand[22:0]};

assign product = operand_a * operand_b;			//Calculating Product

assign product_round = |product_normalised[22:0];  //Ending 22 bits are OR'ed for rounding operation.

assign normalised = product[47] ? 1'b1 : 1'b0;	

assign product_normalised = normalised ? product : product << 1;	//Assigning Normalised value based on 48th bit

//Final Manitssa.
assign product_mantissa = product_normalised[46:24] + (product_normalised[23] & product_round); 

assign zero = Exception ? 1'b0 : (product_mantissa == 23'd0) ? 1'b1 : 1'b0;

assign sum_exponent = a_operand[30:23] + b_operand[30:23];

assign exponent = sum_exponent - 8'd127 + normalised;

assign Overflow = ((exponent[8] & !exponent[7]) & !zero) ; //If overall exponent is greater than 255 then Overflow condition.
//Exception Case when exponent reaches its maximu value that is 384.

//If sum of both exponents is less than 127 then Underflow condition.
assign Underflow = ((exponent[8] & exponent[7]) & !zero) ? 1'b1 : 1'b0; 

assign result = Exception ? 32'd0 : zero ? {sign,31'd0} : Overflow ? {sign,8'hFF,23'd0} : Underflow ? {sign,31'd0} : {sign,exponent[7:0],product_mantissa};
endmodule

  
