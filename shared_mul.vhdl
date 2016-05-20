---------------------------------------------------------------------------
--
-- Copyright (C) 2016 Stiftung Secure Information and 
--                    Communication Technologies SIC and
--                    Graz University of Technology
-- Contact: http://opensource.iaik.tugraz.at
--
-- This file is part of AES DOM.
--
-- $BEGIN_LICENSE:DEFAULT$
-- Commercial License Usage
-- Licensees holding valid commercial licenses may use this file in
-- accordance with the commercial license agreement provided with the
-- Software or, alternatively, in accordance with the terms contained in
-- a written agreement between you and SIC. For further information
-- contact us at http://opensource.iaik.tugraz.at.
--
-- GNU General Public License Usage
-- Alternatively, this file may be used under the terms of the GNU
-- General Public License version 3.0 as published by the Free Software
-- Foundation and appearing in the file LICENSE.GPL included in the
-- packaging of this file.  Please review the following information to
-- ensure the GNU General Public License version 3.0 requirements will be
-- met: http://www.gnu.org/copyleft/gpl.html.
-- 
-- This software is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this software. If not, see http://www.gnu.org/licenses/.
--
-- $END_LICENSE:DEFAULT$
--
----------------------------------------------------------------------------
--       ### shared_mul.vhdl ###
--
--
-- Description:
-- Variants of masked\shared multipliers with varying number of shares
--   
--
-- Initial Version: Date: 27. October 2015 by Hannes Gross IAIK
--
------------------------------------------------------------------------------
library ieee;  use ieee.std_logic_1164.all;
               use work.masked_aes_pkg.all;

entity shared_mul is
  generic (
    PIPELINED  : string  := "no";
    NUM_SHARES : integer := 2;
    DATA_WIDTH : integer := 4
    );
  port (
    -- Clock and reset
    ClkxCI : in  std_logic;
    RstxBI : in  std_logic;
    -- Shares of X and Y
    XxDI   : in  std_logic_vector(DATA_WIDTH*NUM_SHARES-1 downto 0);
    YxDI   : in  std_logic_vector(DATA_WIDTH*NUM_SHARES-1 downto 0);
    -- Fresh masks
    ZxDI   : in  std_logic_vector(DATA_WIDTH*((NUM_SHARES-2)*2 + 1)-1 downto 0);
    -- Output Q = X*Y (+ Z)
    QxDO   : out std_logic_vector(DATA_WIDTH*NUM_SHARES-1 downto 0)
    );
  constant NUM_MASKS : integer := (NUM_SHARES-2)*2 + 1;
end shared_mul;

-------------------------------------------------------------------
architecture behavorial of shared_mul is
  -- Converted inputs
  type t_shared_data is array(NUM_SHARES-1 downto 0) of std_logic_vector(DATA_WIDTH-1 downto 0);
  type t_shared_rand is array(NUM_MASKS-1 downto 0)  of std_logic_vector(DATA_WIDTH-1 downto 0);
  signal XxD : t_shared_data;
  signal YxD : t_shared_data;
  signal QxD : t_shared_data;
  signal ZxD : t_shared_rand;
  
  -- Intermediates
  type t_mult_res is array(NUM_SHARES*NUM_SHARES-1 downto 0) of std_logic_vector(DATA_WIDTH-1 downto 0);
  signal Xi_mul_Yj : t_mult_res;
  
  -- Synchronization FF's
  signal FFxDN   : t_mult_res;
  signal FFxDP   : t_mult_res;
begin

  -------------------------------------------------------------------
  -- General stuff:
  -- Input conversion
  convert_inputs_p: process (QxD, XxDI, YxDI, ZxDI) is
  begin  -- process convert_inputs_p
    -- Convert input shares
    for i in 0 to NUM_SHARES-1 loop
      XxD(i) <= XxDI(DATA_WIDTH*(i+1)-1 downto DATA_WIDTH*i);
      YxD(i) <= YxDI(DATA_WIDTH*(i+1)-1 downto DATA_WIDTH*i);
    end loop;  -- i

    -- Convert input fresh masks
    for i in 0 to NUM_MASKS-1 loop
      ZxD(i) <= ZxDI(DATA_WIDTH*(i+1)-1 downto DATA_WIDTH*i);
    end loop;  -- i

    -- Convert before output
    for i in 0 to NUM_SHARES-1 loop
      QxDO(DATA_WIDTH*(i+1)-1 downto DATA_WIDTH*i) <= QxD(i);
    end loop;  -- i
  end process convert_inputs_p;

  -- Generate multipliers
  gen_inner_multipliers_g : for i in NUM_SHARES-1 downto 0 generate
    gen_outer_multipliers_g : for j in NUM_SHARES-1 downto 0 generate  
      gf2_mul : entity work.gf2_mul
        generic map (
          N => DATA_WIDTH)
        port map (
          AxDI => XxD(i),
          BxDI => YxD(j),
          QxDO => Xi_mul_Yj(NUM_SHARES*i + j));
      end generate gen_outer_multipliers_g;
  end generate gen_inner_multipliers_g;

  -- purpose: Register process
  -- type   : sequential
  -- inputs : ClkxCI, RstxBI
  -- outputs: 
  register_proc_seq : process (ClkxCI, RstxBI) is
  begin  -- process register_proc_seq
    if RstxBI = '0' then                -- asynchronous reset (active low)
      for i in NUM_SHARES-1 downto 0 loop
        for j in NUM_SHARES-1 downto 0 loop
          FFxDP(NUM_SHARES*i + j) <= (others => '0');
        end loop; --j
      end loop; -- i
    elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
      for i in NUM_SHARES-1 downto 0 loop
        for j in NUM_SHARES-1 downto 0 loop
          FFxDP(NUM_SHARES*i + j) <= FFxDN(NUM_SHARES*i + j);
        end loop; --j
      end loop; -- i
    end if;
  end process register_proc_seq;

  ------------------------------------------------------------------
  -- Masked Multiplier Nth order secure for odd number of shares, pipelined
  odd_shares_pipelined : if ((NUM_SHARES mod 2) = 1) and (PIPELINED = "yes") generate
    -- purpose: implements the shared multiplication in a secure and generic way
    -- type   : combinational
    -- inputs : 
    -- outputs: 
    shared_mul_p : process (FFxDP, Xi_mul_Yj, ZxD) is
      variable result : std_logic_vector(DATA_WIDTH-1 downto 0);
    begin  -- process odd_shared_mul_p
      -- iterate over shares
      for i in 0 to NUM_SHARES-1 loop
        result := (others => '0');
        for j in 0 to NUM_SHARES-1 loop
          -- Fi = Xi*Yi + SUM(Xi*Yj + Zj)/j!=i
          if (i = j) then
            FFxDN(NUM_SHARES*i + j) <= Xi_mul_Yj(NUM_SHARES*i + j);             -- domain term
          else
            FFxDN(NUM_SHARES*i + j) <= Xi_mul_Yj(NUM_SHARES*i + j) xor ZxD(j);  -- regular term
          end if;
          -- Output
          result := result xor FFxDP(NUM_SHARES*i + j);
        end loop;  -- j     
        QxD(i) <= result;
      end loop;  -- i
    end process shared_mul_p;
  end generate odd_shares_pipelined;

  -------------------------------------------------------------------
  -- Masked Multiplier Nth order secure for even number of shares, pipelined
  even_shares_pipelined : if ((NUM_SHARES mod 2) = 0) and (PIPELINED = "yes") generate
    -- purpose: implements the shared multiplication in a secure and generic way
    -- type   : combinational
    -- inputs : 
    -- outputs: 
    shared_mul_p : process (FFxDP, FFxDN, Xi_mul_Yj, ZxD) is
      variable result : std_logic_vector(DATA_WIDTH-1 downto 0);
    begin  -- process odd_shared_mul_p
      -- iterate over shares
      for i in 0 to NUM_SHARES-1 loop
        result := (others => '0');
        for j in 0 to NUM_SHARES-1 loop
          -- Fi = Xi*Yi + SUM(Xi*Yj + Z[j+i mod |Z|])/j!=i
          if (i = j) then
            FFxDN(NUM_SHARES*i + j) <= Xi_mul_Yj(NUM_SHARES*i + j);  -- domain term
          else
            FFxDN(NUM_SHARES*i + j) <= Xi_mul_Yj(NUM_SHARES*i + j) xor ZxD( (j+i) mod NUM_MASKS);  -- regular term
          end if;
          -- Output
          result := result xor FFxDP(NUM_SHARES*i + j);
        end loop;  -- j
        QxD(i) <= result;
      end loop;  -- i
    end process shared_mul_p;
  end generate even_shares_pipelined;

  -------------------------------------------------------------------
  -- Masked Multiplier Nth order secure for odd number of shares, pipelined
  odd_shares_not_pipelined : if ((NUM_SHARES mod 2) = 1) and (PIPELINED = "no") generate
    -- purpose: implements the shared multiplication in a secure and generic way
    -- type   : combinational
    -- inputs : 
    -- outputs: 
    shared_mul_p : process (FFxDN, FFxDP, Xi_mul_Yj, ZxD) is
      variable result : std_logic_vector(DATA_WIDTH-1 downto 0);
    begin  -- process odd_shared_mul_p
      -- iterate over shares
      for i in 0 to NUM_SHARES-1 loop
        result := (others => '0');
        for j in 0 to NUM_SHARES-1 loop
          -- Fi = Xi*Yi + SUM(Xi*Yj + Zj)/j!=i
          if (i = j) then
            FFxDN(NUM_SHARES*i + j) <= Xi_mul_Yj(NUM_SHARES*i + j);  -- domain term
          else
            FFxDN(NUM_SHARES*i + j) <= Xi_mul_Yj(NUM_SHARES*i + j) xor ZxD(j);  -- regular term
          end if;
          -- Output
          if (i = j) then
            result := result xor FFxDN(NUM_SHARES*i + j);
          else
            result := result xor FFxDP(NUM_SHARES*i + j);
          end if;
        end loop;  -- j
        QxD(i) <= result;
      end loop;  -- i
    end process shared_mul_p;
  end generate odd_shares_not_pipelined;

  -------------------------------------------------------------------
  -- Masked Multiplier Nth order secure for even number of shares
  even_shares_not_pipelined : if ((NUM_SHARES mod 2) = 0) and (PIPELINED = "no") generate
    -- purpose: implements the shared multiplication in a secure and generic way
    -- type   : combinational
    -- inputs : 
    -- outputs: 
    shared_mul_p : process (FFxDN, FFxDP, Xi_mul_Yj, ZxD)
      variable result : std_logic_vector(DATA_WIDTH-1 downto 0);
    begin  -- process odd_shared_mul_p
      -- iterate over shares
      for i in 0 to NUM_SHARES-1 loop
        result := (others => '0');
        for j in 0 to NUM_SHARES-1 loop
          -- Fi = Xi*Yi + SUM(Xi*Yj + Z[j+i mod |Z|])/j!=i
          if (i = j) then
            FFxDN(NUM_SHARES*i + j) <= Xi_mul_Yj(NUM_SHARES*i + j);  -- domain term
          else
            FFxDN(NUM_SHARES*i + j) <= Xi_mul_Yj(NUM_SHARES*i + j) xor ZxD((j+i) mod NUM_MASKS);  -- regular term
          end if;
          -- Output
          if (i = j) then
            result := result xor FFxDN(NUM_SHARES*i + j);
          else
            result := result xor FFxDP(NUM_SHARES*i + j);
          end if;
        end loop;  -- j
        QxD(i) <= result;
      end loop;  -- i
    end process shared_mul_p;
  end generate even_shares_not_pipelined;

end behavorial;
