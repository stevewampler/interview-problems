package com.sgw.problems.cipher;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

public class DecryptingCaesarTest extends CaesarCipherTest {
  @Override
  void test(String clear, String cipher) {
    assertThat(this.getCipher().decrypt(cipher), is(equalTo(clear)));
  }
}
