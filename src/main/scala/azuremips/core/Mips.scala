package azuremips.core

import spinal.core._

object Mips {
  // I-Type Structure
  def opcodeRange      = 31 downto 26
  def rsRange          = 25 downto 21
  def rtRange          = 20 downto 16
  def immediateRange   = 15 downto 0

  // J-Type Structure
  def instr_indexRange = 25 downto 0

  // R-Type Structure
  def rdRange          = 15 downto 11
  def saRange          = 10 downto 6
  def functionRange    = 5 downto 0

  // Instructions
  def ADD      = M"000000---------------00000100000"
  def ADDI     = M"001000--------------------------"
  def ADDU     = M"000000---------------00000100001"
  def ADDIU    = M"001001--------------------------"
  
}

object Cp0 {
  def BadVAddrIdx = 8
  def CountIdx    = 9
  def StatusIdx   = 12
  def CauseIdx    = 13
}