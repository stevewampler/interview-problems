package com.sgw.problems.cipher;

import org.junit.Before;
import org.junit.Test;

public abstract class CaesarCipherTest {
  public static final int DEFAULT_SHIFT_AMOUNT = 13;
  private CaesarCipherClear cipher;

  @Before
  public void createCipher() {
    cipher = new CaesarCipherClear(DEFAULT_SHIFT_AMOUNT);
  }

  @Test
  public void ignores_empty_string() {
    test("", "");
  }

  @Test
  public void ignores_null_string() {
    test(null, null);
  }

  @Test
  public void ignores_whitespace_string() {
    test("  ", "  ");
  }

  @Test
  public void cipher_shifts_characters() {
    cipher = aCipher(1);

    test("a", "b");
  }

  @Test
  public void cipher_ignores_symbols() {
    test("&", "&");
  }

  @Test
  public void cipher_shifts_numbers() {
    cipher = aCipher(1);

    test("1", "2");
  }

  @Test
  public void cipher_preserves_case() {
    cipher = aCipher(1);

    test("A", "B");
    test("aAa", "bBb");
  }

  @Test
  public void cipher_wraps_at_z() {
    cipher = aCipher(1);

    test("z", "a");
  }

  @Test
  public void cipher_wraps_at_Z() {
    cipher = aCipher(1);

    test("Z", "A");
  }

  @Test
  public void cipher_wraps_at_A() {
    cipher = aCipher(-1);
    test("A", "Z");
  }

  @Test
  public void cipher_wraps_at_a() {
    cipher = aCipher(-1);
    test("a", "z");
  }

  @Test
  public void cipher_wraps_at_9() {
    cipher = aCipher(1);
    test("9", "0");
  }

  @Test
  public void cipher_wraps_at_0() {
    cipher = aCipher(-1);
    test("0", "9");
  }

  @Test
  public void cipher_can_handle_ridiculously_large_shifts() {
    cipher = aCipher(2349);
    test("0", "9");
  }

  abstract void test(String clear, String cipher);


  private CaesarCipherClear aCipher(int shift) {
    return new CaesarCipherClear(shift);
  }

  public CaesarCipherClear getCipher() {
    return cipher;
  }
}
