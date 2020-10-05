LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY display IS
	PORT(	dup, clk, run		      : IN std_logic;	-- duplicate same character
		i0, i1, i2, i3, i4, i5, i6, i7: IN std_logic;	-- input
		q0, q1, q2, q3, q4, q5, q6, q7: OUT std_logic;	-- left display
		w0, w1, w2, w3, w4, w5, w6, w7: OUT std_logic;	-- center display
		e0, e1, e2, e3, e4, e5, e6, e7: OUT std_logic);	-- right display
	END display;

ARCHITECTURE arc OF display IS
	TYPE state_type IS (standby,write,savadd,loadmem,load,disply,move,copy,delete,clear,arithmetic);	-- states of the "computer"
	SIGNAL state: state_type :=standby;				-- the state previously mentioned
	TYPE col IS ARRAY(0 to 6) OF std_logic_vector(7 downto 0);
	TYPE reg IS ARRAY(2 downto 0) OF std_logic_vector(7 downto 0);
	TYPE mmr IS ARRAY(0 to 31) OF std_logic_vector(7 downto 0);
	SIGNAL inp: std_logic_vector(7 downto 0) :="00110000";		-- input vector
	SIGNAL dis: col;						-- display array of vectors (0 to 3 from right to left) and 3 more "invisible" characters
	SIGNAL z: std_logic_vector(3 downto 0) :="0000";		-- the least 4-bit for display 2
	SIGNAL x: std_logic_vector(3 downto 0) :="0000";		-- the least 4-bit for display 1
	SIGNAL c: std_logic_vector(3 downto 0) :="0000";		-- the least 4-bit for display 0
	SIGNAL v: std_logic_vector(3 downto 0) :="0000";		-- the least 4-bit for display 5
	SIGNAL n: std_logic_vector(3 downto 0) :="0000";		-- the least 4-bit for display 4
	SIGNAL m: std_logic_vector(3 downto 0) :="0000";		-- the least 4-bit for display 3
	SIGNAL zi: integer :=0;						-- converting the 4-bit into 0 or 1
	SIGNAL xi: integer :=0;						-- converting the 4-bit into 0 or 1
	SIGNAL ci: integer :=0;						-- converting the 4-bit into 0 or 1
	SIGNAL vi: integer :=0;						-- converting the 4-bit into 0 or 1
	SIGNAL ni: integer :=0;						-- converting the 4-bit into 0 or 1
	SIGNAL mi: integer :=0;						-- converting the 4-bit into 0 or 1
	SIGNAL mad: integer :=0;					-- BCD for display 2 to 0
	SIGNAL val: integer :=0;					-- BCD for display 5 to 3
	SIGNAL mem0: mmr;-- memory address (temporary)
	SIGNAL mem1: mmr;
	SIGNAL mem2: mmr;
	SIGNAL tem: integer :=0;					-- a variable to hold temporary value
	SIGNAL temi: integer :=0;					-- a variable to hold temporary value
	SIGNAL tm0: integer :=0;					-- decomposing tem decimally
	SIGNAL tm1: integer :=0;					-- decomposing tem decimally
	SIGNAL tm2: integer :=0;					-- decomposing tem decimally
	SIGNAL tem0: std_logic_vector(7 downto 0) :="00110000";		-- BCD for tem
	SIGNAL tem1: std_logic_vector(7 downto 0) :="00110000";		-- BCD for tem
	SIGNAL tem2: std_logic_vector(7 downto 0) :="00110000";		-- BCD for tem
	SIGNAL temdisp : reg :=("00110000", "00110000", "00110000");	-- temporary display
	SIGNAL ai0: integer :=0;					-- BCD for register A
	SIGNAL ai1: integer :=0;					-- BCD for register A
	SIGNAL ai2: integer :=0;					-- BCD for register A
	SIGNAL bi0: integer :=0;					-- BCD for register B
	SIGNAL bi1: integer :=0;					-- BCD for register B
	SIGNAL bi2: integer :=0;					-- BCD for register B
	SIGNAL aval : integer :=0;					-- integer for register A
	SIGNAL bval : integer :=0;					-- integer for register B
	SIGNAL a: reg :=("00110000", "00110000", "00110000");		-- register A
	SIGNAL b: reg :=("00110000", "00110000", "00110000");		-- register B

BEGIN
PROCESS(inp,dup,clk)
BEGIN
IF run = '0' THEN
	temi <= 0;
	CASE state IS
	WHEN standby =>
		IF inp = "10111011" THEN
			IF (dis(2) = "01110111" AND dis(1) = "01110010" AND dis(0) = "01110100") THEN
				state <= write;
			ELSIF (dis(2) = "01110011" AND dis(1) = "01100001" AND dis(0) = "01110110") THEN
				state <= savadd;
			ELSIF (dis(2) = "01101100" AND dis(1) = "01101111" AND dis(0) = "01100100") THEN
				state <= load;
			ELSIF (dis(2) = "01100100" AND dis(1) = "01110011" AND dis(0) = "01110000") THEN
				state <= disply;
			ELSIF (dis(2) = "01100011" AND dis(1) = "01101111" AND dis(0) = "01110000") THEN
				state <= copy;
			ELSIF (dis(2) = "01100100" AND dis(1) = "01100101" AND dis(0) = "01101100") THEN
				state <= delete;
			ELSIF (dis(2) = "01100011" AND dis(1) = "01101100" AND dis(0) = "01110010") THEN
				state <= clear;
			ELSIF (dis(2) = "01101101" AND dis(1) = "01101111" AND dis(0) = "01110110") THEN
				state <= move;
			ELSIF (dis(2) = "01110000" AND dis(1) = "01101100" AND dis(0) = "01110011") THEN
				tem <= aval+bval;
				state <= arithmetic;
			ELSIF (dis(2) = "01101101" AND dis(1) = "01101001" AND dis(0) = "01101110") THEN
				tem <= aval-bval;
				state <= arithmetic;
			ELSE
				tem <= mad;
			END IF;
			dis(2) <= "00110000";
			dis(1) <= "00110000";
			dis(0) <= "00110000";
		ELSE
			FOR i IN 6 DOWNTO 1 LOOP
				dis(i) <= dis(i-1);
			END LOOP;
			dis(0) <= inp;
			state <= standby;
		END IF;
	WHEN write =>
		IF inp = "10111011" THEN
			mem2(tem) <= dis(2);
			mem1(tem) <= dis(1);
			mem0(tem) <= dis(0);
			dis(2) <= "00110000";
			dis(1) <= "00110000";
			dis(0) <= "00110000";
			state <= standby;
		ELSE
			dis(2) <= dis(1);
			dis(1) <= dis(0);
			dis(0) <= inp;
			state <= write;
		END IF;
	WHEN savadd =>
		IF inp = "10111011" THEN
			mem2(mad) <= tem2;
			mem1(mad) <= tem1;
			mem0(mad) <= tem0;
			dis(2) <= "00110000";
			dis(1) <= "00110000";
			dis(0) <= "00110000";
			state <= standby;
		ELSE
			dis(2) <= dis(1);
			dis(1) <= dis(0);
			dis(0) <= inp;
			state <= savadd;
		END IF;
	WHEN load =>
		IF inp = "10111011" THEN
			IF (dis(2) = "01101101" AND dis(1) = "01100101" AND dis(0) = "01101101") THEN
				state <= loadmem;
			ELSIF (dis(2) = "01110010" AND dis(1) = "01100111" AND dis(0) = "01100001") THEN
				a(2) <= tem2;
				a(1) <= tem1;
				a(0) <= tem0;
				state <= standby;
			ELSIF (dis(2) = "01110010" AND dis(1) = "01100111" AND dis(0) = "01100010") THEN
				b(2) <= tem2;
				b(1) <= tem1;
				b(0) <= tem0;
				state <= standby;
			ELSE
				state <= load;
			END IF;
			dis(2) <= "00110000";
			dis(1) <= "00110000";
			dis(0) <= "00110000";
		ELSE
			FOR i IN 6 DOWNTO 1 LOOP
				dis(i) <= dis(i-1);
			END LOOP;
			dis(0) <= inp;
			state <= load;
		END IF;
	WHEN loadmem =>
		IF inp = "10111011" THEN
			IF (dis(2) = "01110010" AND dis(1) = "01100111" AND dis(0) = "01100001") THEN
				a(2) <= mem2(tem);
				a(1) <= mem1(tem);
				a(0) <= mem0(tem);
				state <= standby;
			ELSIF (dis(2) = "01110010" AND dis(1) = "01100111" AND dis(0) = "01100010") THEN
				b(2) <= mem2(tem);
				b(1) <= mem1(tem);
				b(0) <= mem0(tem);
				state <= standby;
			ELSE
				tem <= mad;
				state <= loadmem;
			END IF;
			dis(2) <= "00110000";
			dis(1) <= "00110000";
			dis(0) <= "00110000";
		ELSE
			FOR i IN 6 DOWNTO 1 LOOP
				dis(i) <= dis(i-1);
			END LOOP;
			dis(0) <= inp;
			state <= load;
		END IF;
	WHEN disply =>
		IF inp = "10111011" THEN
			IF (dis(2) = "01110010" AND dis(1) = "01100111" AND dis(0) = "01100001") THEN
				FOR i IN 2 DOWNTO 0 LOOP
					dis(i) <= a(i);
				END LOOP;
			ELSIF (dis(2) = "01110010" AND dis(1) = "01100111" AND dis(0) = "01100010") THEN
				FOR i IN 2 DOWNTO 0 LOOP
					dis(i) <= b(i);
				END LOOP;
			ELSE
				dis(2) <= mem2(mad);
				dis(1) <= mem1(mad);
				dis(0) <= mem0(mad);
			END IF;
			state <= standby;
		ELSE
			FOR i IN 6 DOWNTO 1 LOOP
				dis(i) <= dis(i-1);
			END LOOP;
			dis(0) <= inp;
			state <= disply;
		END IF;
	WHEN copy =>
		IF inp = "10111011" THEN
			mem2(mad) <= mem2(tem);
			mem1(mad) <= mem1(tem);
			mem0(mad) <= mem0(tem);
			dis(2) <= "00110000";
			dis(1) <= "00110000";
			dis(0) <= "00110000";
			state <= standby;
		ELSE
			FOR i IN 6 DOWNTO 1 LOOP
				dis(i) <= dis(i-1);
			END LOOP;
			dis(0) <= inp;
			state <= copy;
		END IF;
	WHEN delete =>
		IF inp = "10111011" THEN
			mem2(mad) <= "00110000";
			mem1(mad) <= "00110000";
			mem0(mad) <= "00110000";
			dis(2) <= "00110000";
			dis(1) <= "00110000";
			dis(0) <= "00110000";
			state <= standby;
		ELSE
			FOR i IN 6 DOWNTO 1 LOOP
				dis(i) <= dis(i-1);
			END LOOP;
			dis(0) <= inp;
			state <= delete;
		END IF;
	WHEN clear =>
		FOR i IN 31 DOWNTO 0 LOOP
			mem2(i) <= "00110000";
			mem1(i) <= "00110000";
			mem0(i) <= "00110000";
		END LOOP;
		dis(2) <= "00110000";
		dis(1) <= "00110000";
		dis(0) <= "00110000";
		state <= standby;
	WHEN move =>
		IF inp = "10111011" THEN
			mem2(mad) <= mem2(tem);
			mem1(mad) <= mem1(tem);
			mem0(mad) <= mem0(tem);
			mem2(tem) <= "00110000";
			mem1(tem) <= "00110000";
			mem0(tem) <= "00110000";
			dis(2) <= "00110000";
			dis(1) <= "00110000";
			dis(0) <= "00110000";
			state <= standby;
		ELSE
			FOR i IN 6 DOWNTO 1 LOOP
				dis(i) <= dis(i-1);
			END LOOP;
			dis(0) <= inp;
			state <= move;
		END IF;
	WHEN arithmetic =>
		dis(2) <= tem2;
		dis(1) <= tem1;
		dis(0) <= tem0;
		state <= standby;
		
	END CASE;
ELSE
	CASE state IS
	WHEN standby =>
		IF inp = "10111011" THEN
			IF (dis(2) = "01110011" AND dis(1) = "01100001" AND dis(0) = "01110110") THEN
				state <= savadd;
			ELSIF (dis(2) = "01101100" AND dis(1) = "01101111" AND dis(0) = "01100100") THEN
				state <= load;
			ELSIF (dis(2) = "01100100" AND dis(1) = "01110011" AND dis(0) = "01110000") THEN
				state <= disply;
			ELSIF (dis(2) = "01100011" AND dis(1) = "01101111" AND dis(0) = "01110000") THEN
				state <= copy;
			ELSIF (dis(2) = "01100100" AND dis(1) = "01100101" AND dis(0) = "01101100") THEN
				state <= delete;
			ELSIF (dis(2) = "01100011" AND dis(1) = "01101100" AND dis(0) = "01110010") THEN
				state <= clear;
			ELSIF (dis(2) = "01101101" AND dis(1) = "01101111" AND dis(0) = "01110110") THEN
				state <= move;
			ELSIF (dis(2) = "01110000" AND dis(1) = "01101100" AND dis(0) = "01110011") THEN
				tem <= aval+bval;
				state <= arithmetic;
			ELSIF (dis(2) = "01101101" AND dis(1) = "01101001" AND dis(0) = "01101110") THEN
				tem <= aval-bval;
				state <= arithmetic;				
			ELSE
				tem <= mad;
			END IF;
			dis(2) <= "00110000";
			dis(1) <= "00110000";
			dis(0) <= "00110000";
		ELSE
			FOR i IN 6 DOWNTO 1 LOOP
				dis(i) <= dis(i-1);
			END LOOP;
			dis(0) <= inp;
			state <= standby;
		END IF;
	WHEN savadd =>
		IF inp = "10111011" THEN
			mem2(mad) <= tem2;
			mem1(mad) <= tem1;
			mem0(mad) <= tem0;
			dis(2) <= "00110000";
			dis(1) <= "00110000";
			dis(0) <= "00110000";
			state <= standby;
		ELSE
			dis(2) <= dis(1);
			dis(1) <= dis(0);
			dis(0) <= inp;
			state <= savadd;
		END IF;
	WHEN load =>
		IF inp = "10111011" THEN
			IF (dis(2) = "01101101" AND dis(1) = "01100101" AND dis(0) = "01101101") THEN
				state <= loadmem;
			ELSIF (dis(2) = "01110010" AND dis(1) = "01100111" AND dis(0) = "01100001") THEN
				a(2) <= tem2;
				a(1) <= tem1;
				a(0) <= tem0;
				state <= standby;
			ELSIF (dis(2) = "01110010" AND dis(1) = "01100111" AND dis(0) = "01100010") THEN
				b(2) <= tem2;
				b(1) <= tem1;
				b(0) <= tem0;
				state <= standby;
			ELSE
				state <= load;
			END IF;
			dis(2) <= "00110000";
			dis(1) <= "00110000";
			dis(0) <= "00110000";
		ELSE
			FOR i IN 6 DOWNTO 1 LOOP
				dis(i) <= dis(i-1);
			END LOOP;
			dis(0) <= inp;
			state <= load;
		END IF;
	WHEN loadmem =>
		IF inp = "10111011" THEN
			IF (dis(2) = "01110010" AND dis(1) = "01100111" AND dis(0) = "01100001") THEN
				a(2) <= mem2(tem);
				a(1) <= mem1(tem);
				a(0) <= mem0(tem);
				state <= standby;
			ELSIF (dis(2) = "01110010" AND dis(1) = "01100111" AND dis(0) = "01100010") THEN
				b(2) <= mem2(tem);
				b(1) <= mem1(tem);
				b(0) <= mem0(tem);
				state <= standby;
			ELSE
				tem <= mad;
				state <= loadmem;
			END IF;
			dis(2) <= "00110000";
			dis(1) <= "00110000";
			dis(0) <= "00110000";
		ELSE
			FOR i IN 6 DOWNTO 1 LOOP
				dis(i) <= dis(i-1);
			END LOOP;
			dis(0) <= inp;
			state <= load;
		END IF;
	WHEN disply =>
		IF inp = "10111011" THEN
			IF (dis(2) = "01110010" AND dis(1) = "01100111" AND dis(0) = "01100001") THEN
				FOR i IN 2 DOWNTO 0 LOOP
					dis(i) <= a(i);
				END LOOP;
			ELSIF (dis(2) = "01110010" AND dis(1) = "01100111" AND dis(0) = "01100010") THEN
				FOR i IN 2 DOWNTO 0 LOOP
					dis(i) <= b(i);
				END LOOP;
			ELSE
				dis(2) <= mem2(mad);
				dis(1) <= mem1(mad);
				dis(0) <= mem0(mad);
			END IF;
			state <= standby;
		ELSE
			FOR i IN 6 DOWNTO 1 LOOP
				dis(i) <= dis(i-1);
			END LOOP;
			dis(0) <= inp;
			state <= disply;
		END IF;
	WHEN copy =>
		IF inp = "10111011" THEN
			mem2(mad) <= mem2(tem);
			mem1(mad) <= mem1(tem);
			mem0(mad) <= mem0(tem);
			dis(2) <= "00110000";
			dis(1) <= "00110000";
			dis(0) <= "00110000";
			state <= standby;
		ELSE
			FOR i IN 6 DOWNTO 1 LOOP
				dis(i) <= dis(i-1);
			END LOOP;
			dis(0) <= inp;
			state <= copy;
		END IF;
	WHEN delete =>
		IF inp = "10111011" THEN
			mem2(mad) <= "00110000";
			mem1(mad) <= "00110000";
			mem0(mad) <= "00110000";
			dis(2) <= "00110000";
			dis(1) <= "00110000";
			dis(0) <= "00110000";
			state <= standby;
		ELSE
			FOR i IN 6 DOWNTO 1 LOOP
				dis(i) <= dis(i-1);
			END LOOP;
			dis(0) <= inp;
			state <= delete;
		END IF;
	WHEN clear =>
		FOR i IN 7 DOWNTO 0 LOOP
			mem2(i) <= "00110000";
			mem1(i) <= "00110000";
			mem0(i) <= "00110000";
		END LOOP;
		dis(2) <= "00110000";
		dis(1) <= "00110000";
		dis(0) <= "00110000";
		state <= standby;
	WHEN move =>
		IF inp = "10111011" THEN
			mem2(mad) <= mem2(tem);
			mem1(mad) <= mem1(tem);
			mem0(mad) <= mem0(tem);
			mem2(tem) <= "00110000";
			mem1(tem) <= "00110000";
			mem0(tem) <= "00110000";
			dis(2) <= "00110000";
			dis(1) <= "00110000";
			dis(0) <= "00110000";
			state <= standby;
		ELSE
			FOR i IN 6 DOWNTO 1 LOOP
				dis(i) <= dis(i-1);
			END LOOP;
			dis(0) <= inp;
			state <= move;
		END IF;
	WHEN arithmetic =>
		dis(2) <= tem2;
		dis(1) <= tem1;
		dis(0) <= tem0;
		state <= standby;
		
	END CASE;
	dis(2) <= mem2(temi);
	dis(1) <= mem1(temi);
	dis(0) <= mem0(temi);
	temi <= temi + 1;
	IF temi = 31 THEN
		temi <= 0;
	END IF;
	IF (dis(2) = "01101100" AND dis(1) = "01101111" AND dis(0) = "01110000") THEN
		temi <= tem;
	END IF;
END IF;
END PROCESS;

-- converting display into BCD and integer

v <= dis(5)(3 downto 0);
n <= dis(4)(3 downto 0);
m <= dis(3)(3 downto 0);
z <= dis(2)(3 downto 0);
x <= dis(1)(3 downto 0);
c <= dis(0)(3 downto 0);

val <= vi*100+ni*10+mi;

WITH v SELECT
vi <= 	1 WHEN "0001",
	2 WHEN "0010",
	3 WHEN "0011",
	4 WHEN "0100",
	5 WHEN "0101",
	6 WHEN "0110",
	7 WHEN "0111",
	8 WHEN "1000",
	9 WHEN "1001",
	0 WHEN OTHERS;

WITH n SELECT
ni <= 	1 WHEN "0001",
	2 WHEN "0010",
	3 WHEN "0011",
	4 WHEN "0100",
	5 WHEN "0101",
	6 WHEN "0110",
	7 WHEN "0111",
	8 WHEN "1000",
	9 WHEN "1001",
	0 WHEN OTHERS;

WITH m SELECT
mi <= 	1 WHEN "0001",
	2 WHEN "0010",
	3 WHEN "0011",
	4 WHEN "0100",
	5 WHEN "0101",
	6 WHEN "0110",
	7 WHEN "0111",
	8 WHEN "1000",
	9 WHEN "1001",
	0 WHEN OTHERS;

WITH z SELECT
zi <= 	1 WHEN "0001",
	2 WHEN "0010",
	3 WHEN "0011",
	4 WHEN "0100",
	5 WHEN "0101",
	6 WHEN "0110",
	7 WHEN "0111",
	8 WHEN "1000",
	9 WHEN "1001",
	0 WHEN OTHERS;

WITH x SELECT
xi <= 	1 WHEN "0001",
	2 WHEN "0010",
	3 WHEN "0011",
	4 WHEN "0100",
	5 WHEN "0101",
	6 WHEN "0110",
	7 WHEN "0111",
	8 WHEN "1000",
	9 WHEN "1001",
	0 WHEN OTHERS;

WITH c SELECT
ci <= 	1 WHEN "0001",
	2 WHEN "0010",
	3 WHEN "0011",
	4 WHEN "0100",
	5 WHEN "0101",
	6 WHEN "0110",
	7 WHEN "0111",
	8 WHEN "1000",
	9 WHEN "1001",
	0 WHEN OTHERS;

mad <= zi*100+xi*10+ci;

-- register in BCD and integer form

WITH a(0)(3 downto 0) SELECT
ai0 <=	1 WHEN "0001",
	2 WHEN "0010",
	3 WHEN "0011",
	4 WHEN "0100",
	5 WHEN "0101",
	6 WHEN "0110",
	7 WHEN "0111",
	8 WHEN "1000",
	9 WHEN "1001",
	0 WHEN OTHERS;

WITH a(1)(3 downto 0) SELECT
ai1 <=	1 WHEN "0001",
	2 WHEN "0010",
	3 WHEN "0011",
	4 WHEN "0100",
	5 WHEN "0101",
	6 WHEN "0110",
	7 WHEN "0111",
	8 WHEN "1000",
	9 WHEN "1001",
	0 WHEN OTHERS;

WITH a(2)(3 downto 0) SELECT
ai2 <=	1 WHEN "0001",
	2 WHEN "0010",
	3 WHEN "0011",
	4 WHEN "0100",
	5 WHEN "0101",
	6 WHEN "0110",
	7 WHEN "0111",
	8 WHEN "1000",
	9 WHEN "1001",
	0 WHEN OTHERS;

WITH b(0)(3 downto 0) SELECT
bi0 <=	1 WHEN "0001",
	2 WHEN "0010",
	3 WHEN "0011",
	4 WHEN "0100",
	5 WHEN "0101",
	6 WHEN "0110",
	7 WHEN "0111",
	8 WHEN "1000",
	9 WHEN "1001",
	0 WHEN OTHERS;

WITH b(1)(3 downto 0) SELECT
bi1 <=	1 WHEN "0001",
	2 WHEN "0010",
	3 WHEN "0011",
	4 WHEN "0100",
	5 WHEN "0101",
	6 WHEN "0110",
	7 WHEN "0111",
	8 WHEN "1000",
	9 WHEN "1001",
	0 WHEN OTHERS;

WITH b(2)(3 downto 0) SELECT
bi2 <=	1 WHEN "0001",
	2 WHEN "0010",
	3 WHEN "0011",
	4 WHEN "0100",
	5 WHEN "0101",
	6 WHEN "0110",
	7 WHEN "0111",
	8 WHEN "1000",
	9 WHEN "1001",
	0 WHEN OTHERS;

aval <= 100*ai2+10*ai1+ai0;
bval <= 100*bi2+10*bi1+bi0;

-- temporary vector from tem variable

tm0 <= tem mod 10;
tm1 <= ((tem-tm0)/10) mod 10;
tm2 <= ((((tem-tm0)/10)-tm1)/10) mod 10;

WITH tm0 SELECT
tem0 <= "00110001" WHEN 1,
	"00110010" WHEN 2,
	"00110011" WHEN 3,
	"00110100" WHEN 4,
	"00110101" WHEN 5,
	"00110110" WHEN 6,
	"00110111" WHEN 7,
	"00111000" WHEN 8,
	"00111001" WHEN 9,
	"00110000" WHEN OTHERS;

WITH tm1 SELECT
tem1 <= "00110001" WHEN 1,
	"00110010" WHEN 2,
	"00110011" WHEN 3,
	"00110100" WHEN 4,
	"00110101" WHEN 5,
	"00110110" WHEN 6,
	"00110111" WHEN 7,
	"00111000" WHEN 8,
	"00111001" WHEN 9,
	"00110000" WHEN OTHERS;

WITH tm2 SELECT
tem2 <= "00110001" WHEN 1,
	"00110010" WHEN 2,
	"00110011" WHEN 3,
	"00110100" WHEN 4,
	"00110101" WHEN 5,
	"00110110" WHEN 6,
	"00110111" WHEN 7,
	"00111000" WHEN 8,
	"00111001" WHEN 9,
	"00110000" WHEN OTHERS;

-- assigning input and output to signals

inp(0) <= i0;
inp(1) <= i1;
inp(2) <= i2;
inp(3) <= i3;
inp(4) <= i4;
inp(5) <= i5;
inp(6) <= i6;
inp(7) <= i7;

q0 <= dis(2)(0);
q1 <= dis(2)(1);
q2 <= dis(2)(2);
q3 <= dis(2)(3);
q4 <= dis(2)(4);
q5 <= dis(2)(5);
q6 <= dis(2)(6);
q7 <= dis(2)(7);

w0 <= dis(1)(0);
w1 <= dis(1)(1);
w2 <= dis(1)(2);
w3 <= dis(1)(3);
w4 <= dis(1)(4);
w5 <= dis(1)(5);
w6 <= dis(1)(6);
w7 <= dis(1)(7);

e0 <= dis(0)(0);
e1 <= dis(0)(1);
e2 <= dis(0)(2);
e3 <= dis(0)(3);
e4 <= dis(0)(4);
e5 <= dis(0)(5);
e6 <= dis(0)(6);
e7 <= dis(0)(7);
END arc;
