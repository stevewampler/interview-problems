package com.sgw.problems.cipher;

public class CaesarCipherClear {
  private final int shift;

  private final static CharacterShifter[] shifters = {
    new CharacterShifter('A', 26),
    new CharacterShifter('a', 26),
    new CharacterShifter('0', 10)
  };

  public CaesarCipherClear(int shift) {
    this.shift = shift;
  }

  private static char encrypt(char c, int shift) {
    for (CharacterShifter range : shifters) {
      if (range.isInRange(c)) {
        return range.shiftChar(c, shift);
      }
    }

    return c;
  }

  private static String encrypt(String s, int shift) {
    if (s == null) {
      return s;
    }

    StringBuilder builder = new StringBuilder();

    for (int i = 0; i < s.length(); i++) {
      char c = s.charAt(i);

      builder.append(encrypt(c, shift));
    }

    return builder.toString();
  }

  public String encrypt(String s) {
    return encrypt(s, shift);
  }

  public String decrypt(String s) {
    return encrypt(s, -shift);
  }
}
