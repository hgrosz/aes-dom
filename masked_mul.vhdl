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
--       ### masked_mul.vhdl ###
--
--
-- Description:
-- Different variants of masked multipliers 
--   
--
-- Initial Version: Date: 22. September 2015 by Hannes Gross IAIK
--
------------------------------------------------------------------------------
library ieee;  use ieee.std_logic_1164.all; 

entity masked_mul is
  generic ( 
    VARIANT       : string  := "Hybrid"; -- or "Classic"  Implementation Variant
    PIPELINED     : string  := "no";
    USE_TWO_MASKS : string  := "no";    -- or "no", two masks saves one FF at output
    DATA_WIDTH    : integer := 4
  );
  port (  
    -- Clock and reset
    ClkxCI : in std_logic;
    RstxBI : in std_logic;
    -- Shares and masks
    AmxDI  : in std_logic_vector(DATA_WIDTH-1 downto 0); -- share A
    MaxDI  : in std_logic_vector(DATA_WIDTH-1 downto 0); -- mask of A
    BmxDI  : in std_logic_vector(DATA_WIDTH-1 downto 0); -- share B
    MbxDI  : in std_logic_vector(DATA_WIDTH-1 downto 0); -- mask of B
    Mq1xDI : in std_logic_vector(DATA_WIDTH-1 downto 0); -- remasking mask
    Mq2xDI : in std_logic_vector(DATA_WIDTH-1 downto 0); -- optional 2nd mask
    -- Outputs
    QmxDO  : out std_logic_vector(DATA_WIDTH-1 downto 0); -- share Q
    MqxDO  : out std_logic_vector(DATA_WIDTH-1 downto 0)  -- mask of Q
  );
end masked_mul;

-------------------------------------------------------------------

architecture behavorial of masked_mul is 
  signal QmxDN, QmxDP : std_logic_vector(DATA_WIDTH-1 downto 0);
  signal MqxDN, MqxDP : std_logic_vector(DATA_WIDTH-1 downto 0);
  -- Intermediates
  signal I1xD, I2xD, I3xD, I4xD : std_logic_vector(DATA_WIDTH-1 downto 0);
  signal I5xD, I6xD, I7xD       : std_logic_vector(DATA_WIDTH-1 downto 0);
  -- Synchrionization FF's
  signal FF1xDN, FF2xDN, FF3xDN, FF4xDN : std_logic_vector(DATA_WIDTH-1 downto 0);
  signal FF1xDP, FF2xDP, FF3xDP, FF4xDP : std_logic_vector(DATA_WIDTH-1 downto 0);
begin
   -------------------------------------------------------------------
  -- Hybrid Masked Multiplier, TI like with two shares, and one remasking mask
  -- (pipelined)
  hybrid_mul_variant_two_masks_pipelined: if (VARIANT = "Hybrid") and (USE_TWO_MASKS = "yes")  and (PIPELINED = "yes") generate

    -- I1 = Am * Bm
    gf2_mul_1: entity work.gf2_mul
      generic map (
        N => DATA_WIDTH)
      port map (
        AxDI => AmxDI,
        BxDI => BmxDI,
        QxDO => I1xD);

    -- I2 = Bm * Ma
    gf2_mul_2: entity work.gf2_mul
      generic map (
        N => DATA_WIDTH)
      port map (
        AxDI => BmxDI,
        BxDI => MaxDI,
        QxDO => I2xD);

    -- I3 = Am * Mb
    gf2_mul_3: entity work.gf2_mul
      generic map (
        N => DATA_WIDTH)
      port map (
        AxDI => AmxDI,
        BxDI => MbxDI,
        QxDO => I3xD);

    -- I4 = Mb * Ma
    gf2_mul_4: entity work.gf2_mul
      generic map (
        N => DATA_WIDTH)
      port map (
        AxDI => MbxDI,
        BxDI => MaxDI,
        QxDO => I4xD);

    -- purpose: Addup signals
    -- type   : combinational
    adders_comb: process (FF1xDP, FF2xDP, FF3xDP, FF4xDP, I1xD, I2xD, I3xD,
                          I4xD, Mq1xDI, Mq2xDI) is
    begin  -- process adders_comb
      FF4xDN <= I4xD xor Mq1xDI; 
      FF3xDN <= I3xD xor Mq2xDI; 
      FF2xDN <= I2xD xor Mq2xDI;
      FF1xDN <= I1xD xor Mq1xDI;

      -- do not use flip flop at output
      MqxDP <= FF2xDP xor FF1xDP; -- this is the new mask
      QmxDP <= FF4xDP xor FF3xDP; -- masked data
    end process adders_comb;
  end generate hybrid_mul_variant_two_masks_pipelined;

  -------------------------------------------------------------------
  -- Hybrid Masked Multiplier, TI like with two shares, and one remasking mask,
  -- (pipelined)
  hybrid_mul_variant_one_mask_pipelined: if (VARIANT = "Hybrid") and (USE_TWO_MASKS = "no") and (PIPELINED = "yes") generate

    -- I1 = Am * Bm
    gf2_mul_1: entity work.gf2_mul
      generic map (
        N => DATA_WIDTH)
      port map (
        AxDI => AmxDI,
        BxDI => BmxDI,
        QxDO => I1xD);

    -- I2 = Bm * Ma
    gf2_mul_2: entity work.gf2_mul
      generic map (
        N => DATA_WIDTH)
      port map (
        AxDI => BmxDI,
        BxDI => MaxDI,
        QxDO => I2xD);

    -- I3 = Am * Mb
    gf2_mul_3: entity work.gf2_mul
      generic map (
        N => DATA_WIDTH)
      port map (
        AxDI => AmxDI,
        BxDI => MbxDI,
        QxDO => I3xD);

    -- I4 = Mb * Ma
    gf2_mul_4: entity work.gf2_mul
      generic map (
        N => DATA_WIDTH)
      port map (
        AxDI => MbxDI,
        BxDI => MaxDI,
        QxDO => I4xD);

    -- purpose: Addup signals
    -- type   : combinational
    adders_comb: process (FF1xDP, FF2xDP, FF3xDP, FF4xDP, I1xD, I2xD, I3xD,
                          I4xD, Mq1xDI) is
    begin  -- process adders_comb
      FF4xDN <= I2xD xor Mq1xDI; 
      FF3xDN <= I1xD; 
      FF2xDN <= I4xD;
      FF1xDN <= I3xD xor Mq1xDI;

      --MqxDN <= FF2xDP xor FF1xDP; -- this is the new mask
      --QmxDN <= FF4xDP xor FF3xDP; -- masked data
      MqxDP <= FF2xDP xor FF1xDP; -- this is the new mask
      QmxDP <= FF4xDP xor FF3xDP; -- masked data
    end process adders_comb;

    -- purpose: Register process for optional output FF
    -- type   : sequential
    -- inputs : ClkxCI, RstxBI
    -- outputs: 
    --register_proc_seq_opt : process (ClkxCI, RstxBI) is
    --begin  -- process register_proc_seq
    --  if RstxBI = '0' then              -- asynchronous reset (active low)
    --    QmxDP  <= (others => '0');
    --    MqxDP  <= (others => '0');
    --  elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
    --    QmxDP  <= QmxDN;
    --    MqxDP  <= MqxDN;
    --  end if;
    --end process register_proc_seq_opt;
  end generate hybrid_mul_variant_one_mask_pipelined;
  
  -------------------------------------------------------------------
  -- Hybrid Masked Multiplier, TI like with two shares, and one remasking mask
  hybrid_mul_variant_two_masks: if (VARIANT = "Hybrid") and (USE_TWO_MASKS = "yes") and (PIPELINED = "no") generate

    -- I1 = Am * Bm
    gf2_mul_1: entity work.gf2_mul
      generic map (
        N => DATA_WIDTH)
      port map (
        AxDI => AmxDI,
        BxDI => BmxDI,
        QxDO => I1xD);

    -- I2 = Bm * Ma
    gf2_mul_2: entity work.gf2_mul
      generic map (
        N => DATA_WIDTH)
      port map (
        AxDI => BmxDI,
        BxDI => MaxDI,
        QxDO => I2xD);

    -- I3 = Am * Mb
    gf2_mul_3: entity work.gf2_mul
      generic map (
        N => DATA_WIDTH)
      port map (
        AxDI => AmxDI,
        BxDI => MbxDI,
        QxDO => I3xD);

    -- I4 = Mb * Ma
    gf2_mul_4: entity work.gf2_mul
      generic map (
        N => DATA_WIDTH)
      port map (
        AxDI => MbxDI,
        BxDI => MaxDI,
        QxDO => I4xD);

    -- purpose: Addup signals
    -- type   : combinational
    adders_comb: process (FF1xDP, FF2xDP, FF3xDP, FF4xDP, I1xD, I2xD, I3xD,
                          I4xD, Mq1xDI, Mq2xDI) is
    begin  -- process adders_comb
      FF4xDN <= I4xD xor Mq1xDI; 
      FF3xDN <= I3xD xor Mq2xDI; 
      FF2xDN <= I2xD xor Mq2xDI;
      FF1xDN <= I1xD xor Mq1xDI;

      -- do not use flip flop at output
      MqxDP <= FF2xDP xor FF1xDP; -- this is the new mask
      QmxDP <= FF4xDP xor FF3xDP; -- masked data
    end process adders_comb;
  end generate hybrid_mul_variant_two_masks;

  -------------------------------------------------------------------
  -- Hybrid Masked Multiplier, TI like with two shares, and one remasking mask
  hybrid_mul_variant_one_mask: if (VARIANT = "Hybrid") and (USE_TWO_MASKS = "no") and (PIPELINED = "no") generate

    -- I1 = Am * Bm
    gf2_mul_1: entity work.gf2_mul
      generic map (
        N => DATA_WIDTH)
      port map (
        AxDI => AmxDI,
        BxDI => BmxDI,
        QxDO => I1xD);

    -- I2 = Bm * Ma
    gf2_mul_2: entity work.gf2_mul
      generic map (
        N => DATA_WIDTH)
      port map (
        AxDI => BmxDI,
        BxDI => MaxDI,
        QxDO => I2xD);

    -- I3 = Am * Mb
    gf2_mul_3: entity work.gf2_mul
      generic map (
        N => DATA_WIDTH)
      port map (
        AxDI => AmxDI,
        BxDI => MbxDI,
        QxDO => I3xD);

    -- I4 = Mb * Ma
    gf2_mul_4: entity work.gf2_mul
      generic map (
        N => DATA_WIDTH)
      port map (
        AxDI => MbxDI,
        BxDI => MaxDI,
        QxDO => I4xD);

    -- purpose: Addup signals
    -- type   : combinational
    adders_comb: process (FF1xDP, FF2xDP, FF3xDP, FF4xDP, I1xD, I2xD, I3xD,
                          I4xD, Mq1xDI) is
    begin  -- process adders_comb
      FF4xDN <= I2xD xor Mq1xDI; 
      --FF3xDN <= I3xD; 
      --FF2xDN <= I2xD;
      FF1xDN <= I3xD xor Mq1xDI;

      --MqxDN <= FF2xDP xor FF1xDP; -- this is the new mask
      --QmxDN <= FF4xDP xor FF3xDP; -- masked data
      MqxDP <= I1xD xor FF1xDP; -- this is the new mask
      QmxDP <= FF4xDP xor I4xD; -- masked data
    end process adders_comb;

    -- purpose: Register process for optional output FF
    -- type   : sequential
    -- inputs : ClkxCI, RstxBI
    -- outputs: 
    --register_proc_seq_opt : process (ClkxCI, RstxBI) is
    --begin  -- process register_proc_seq
    --  if RstxBI = '0' then              -- asynchronous reset (active low)
    --    QmxDP  <= (others => '0');
    --    MqxDP  <= (others => '0');
    --  elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
    --    QmxDP  <= QmxDN;
    --    MqxDP  <= MqxDN;
    --  end if;
    --end process register_proc_seq_opt;
  end generate hybrid_mul_variant_one_mask;
  
-------------------------------------------------------------------
  -- Classic Masked Multiplier
  classic_mul_variant: if VARIANT = "Classic" generate

    -- I1 = Am * Bm
    gf2_mul_1: entity work.gf2_mul
      generic map (
        N => DATA_WIDTH)
      port map (
        AxDI => AmxDI,
        BxDI => BmxDI,
        QxDO => I1xD);

    -- I2 = Bm * Ma
    gf2_mul_2: entity work.gf2_mul
      generic map (
        N => DATA_WIDTH)
      port map (
        AxDI => BmxDI,
        BxDI => MaxDI,
        QxDO => I2xD);

    -- I3 = Am * Mb
    gf2_mul_3: entity work.gf2_mul
      generic map (
        N => DATA_WIDTH)
      port map (
        AxDI => AmxDI,
        BxDI => MbxDI,
        QxDO => I3xD);

    -- I4 = Mb * Ma
    gf2_mul_4: entity work.gf2_mul
      generic map (
        N => DATA_WIDTH)
      port map (
        AxDI => MbxDI,
        BxDI => MaxDI,
        QxDO => I4xD);

    -- purpose: Addup signals
    -- type   : combinational
    adders_comb: process (I1xD, I2xD, I3xD, I4xD, I5xD, I6xD, I7xD, Mq1xDI) is
    begin  -- process adders_comb
      I7xD  <= I4xD xor Mq1xDI; 
      I6xD  <= I3xD xor I7xD; 
      I5xD  <= I2xD xor I6xD;
      QmxDN <= I1xD xor I5xD;
      MqxDN <= Mq1xDI;
    end process adders_comb;

    -- purpose: Register process for optional output FF
    -- type   : sequential
    -- inputs : ClkxCI, RstxBI
    -- outputs: 
    register_proc_seq_opt : process (ClkxCI, RstxBI) is
    begin  -- process register_proc_seq
      if RstxBI = '0' then              -- asynchronous reset (active low)
        QmxDP  <= (others => '0');
        MqxDP  <= (others => '0');
      elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
        QmxDP  <= QmxDN;
        MqxDP  <= MqxDN;
      end if;
    end process register_proc_seq_opt;
  end generate classic_mul_variant;
  
  -- purpose: Register process
  -- type   : sequential
  -- inputs : ClkxCI, RstxBI
  -- outputs: 
  register_proc_seq: process (ClkxCI, RstxBI) is
  begin  -- process register_proc_seq
    if RstxBI = '0' then                -- asynchronous reset (active low)
      FF1xDP    <= (others => '0');
      FF2xDP    <= (others => '0');
      FF3xDP    <= (others => '0');
      FF4xDP    <= (others => '0');
    elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
      FF1xDP    <= FF1xDN;
      FF2xDP    <= FF2xDN;
      FF3xDP    <= FF3xDN;
      FF4xDP    <= FF4xDN;
    end if;
  end process register_proc_seq;

  -- Outputs
   QmxDO <= QmxDP;
   MqxDO <= MqxDP;
end behavorial;
