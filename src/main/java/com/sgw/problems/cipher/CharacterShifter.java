package com.sgw.problems.cipher;

public class CharacterShifter {
  private char offset;
  private int range;

  public CharacterShifter(char offset, int range) {
    this.offset = offset;
    this.range = range;
  }

  public boolean isInRange(char c) {
    return c >= offset && c < (offset + range);
  }

  public char shiftChar(char c, int shift) {
    if (!isInRange(c)) {
      throw new RuntimeException("Can't shift character '" + c + "' with this shifter: " + this);
    }

    int i = (int) c;

    i = i - offset; // translate to origin

    i = i + (shift % range); // shift it

    if (i < 0) {
      i = range + i;
    } else if (i > (range - 1)) {
      i = i - range;
    }

    i = i + offset;

    return (char) i;
  }
}
