package com.sgw.problems.cipher;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.util.Arrays;
import java.util.Collection;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

@RunWith(Parameterized.class)
public class CaesarParameterizedTest {


  @Parameterized.Parameters(name = "{index}: encrypt({0})={1} && decrypt({1})={0}")
  public static Collection<Object[]> data() {
    return Arrays.asList(
    new Object[][]{
    {1, "", ""},
    {1, " ", " "},
    {1, ",", ","},
    {1, "aba", "bcb"},
    {1, "aBa", "bCb"},
    {13, "Hello, world!", "Uryyb, jbeyq!"},
    {13, "0", "3" },
    {13, "Ideally, the developer writing a test has control of all of the forces that might cause a test to fail.",
    "Vqrnyyl, gur qrirybcre jevgvat n grfg unf pbageby bs nyy bs gur sbeprf gung zvtug pnhfr n grfg gb snvy."},
    {13, "My birthday is 12/17/1974", "Zl oveguqnl vf 45/40/4207"},
    {-13, "Hello, world!", "Uryyb, jbeyq!"},
    {-13, "Ideally, the developer writing a test has control of all of the forces that might cause a test to fail.",
    "Vqrnyyl, gur qrirybcre jevgvat n grfg unf pbageby bs nyy bs gur sbeprf gung zvtug pnhfr n grfg gb snvy."},
    {5, "1234567890", "6789012345"},
    {-5, "1234567890", "6789012345"}
    });
  }


  private CaesarCipherClear cipher;
  private String clearText;
  private String cipherText;

  public CaesarParameterizedTest(int shiftAmount, String clearText, String cipherText) {
    this.cipher = new CaesarCipherClear(shiftAmount);
    this.clearText = clearText;
    this.cipherText = cipherText;
  }

  @Test
  public void encrypts() {
    assertThat(cipher.encrypt(clearText), is(equalTo(cipherText)));
  }

  @Test
  public void decrypts() {
    assertThat(cipher.decrypt(cipherText), is(equalTo(clearText)));
  }

}

