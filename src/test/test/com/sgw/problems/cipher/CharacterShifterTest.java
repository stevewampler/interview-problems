package com.sgw.problems.cipher;

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class CharacterShifterTest {
  @Test
  public void isInRangeReturnsFalseIfCharNotInRange() {
    CharacterShifter cs = new CharacterShifter('A', 26);

    assertFalse(cs.isInRange('a'));
  }

  @Test
  public void isInRangeReturnsTrueIfCharInRange() {
    CharacterShifter cs1 = new CharacterShifter('A', 26);

    assertTrue(cs1.isInRange('A'));
    assertTrue(cs1.isInRange('Z'));

    CharacterShifter cs2 = new CharacterShifter('a', 26);

    assertTrue(cs2.isInRange('a'));
    assertTrue(cs2.isInRange('z'));

    CharacterShifter cs3 = new CharacterShifter('0', 10);

    assertTrue(cs3.isInRange('0'));
    assertTrue(cs3.isInRange('9'));
  }

  @Test
  public void shiftCharShiftsCorrectly() {
    CharacterShifter cs = new CharacterShifter('A', 26);

    assertEquals('B', cs.shiftChar('A', 1));
    assertEquals('Z', cs.shiftChar('A', -1));
    assertEquals('A', cs.shiftChar('Z', 1));

    assertEquals('B', cs.shiftChar('A', 27));
    assertEquals('A', cs.shiftChar('B', -27));
  }
}
