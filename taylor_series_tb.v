module tb_exp;

  reg [31:0] x;        // Input for x (e^x)
  wire [31:0] out;     // Output for e^x

  // Instantiate the exp module
  exp uut (
    .x(x),
    .out(out)
  );

  // Initialize and apply test vectors
  initial begin
    // Monitor the output for any changes
    $monitor("At time %t, x = %h, e^x = %h", $time, x, out);

    // Test case 1: x = 0 (e^0 = 1)
    x = 32'b00111111100000000000000000000000; // x = 0.0 (in IEEE 754 format)
    #10; // Wait for 10 time units

    // Test case 2: x = 1 (e^1 ≈ 2.718)
    x = 32'b00111111101100000000000000000000; // x = 1.0 (in IEEE 754 format)
    #10; // Wait for 10 time units

    // Test case 3: x = -1 (e^-1 ≈ 0.3679)
    x = 32'b00111111000000000000000000000000; // x = -1.0 (in IEEE 754 format)
    #10; // Wait for 10 time units

    // Test case 4: x = 2 (e^2 ≈ 7.389)
    x = 32'b00111111110000000000000000000000; // x = 2.0 (in IEEE 754 format)
    #10; // Wait for 10 time units

    // Test case 5: x = -2 (e^-2 ≈ 0.1353)
    x = 32'b00111110111100000000000000000000; // x = -2.0 (in IEEE 754 format)
    #10; // Wait for 10 time units

    // Test case 6: x = 5 (e^5 ≈ 148.413)
    x = 32'b00111111111110000000000000000000; // x = 5.0 (in IEEE 754 format)
    #10; // Wait for 10 time units

    // Finish simulation
    $finish;
  end
endmodule
