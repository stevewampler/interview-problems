package com.sgw.utils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;

public class IOUtils {
  public interface LineCallback {
    void op(String line);
  }

  public static void readLinesFromResource(String resourcePath, LineCallback callback) throws IOException {
    InputStream inputStream = IOUtils.class.getResourceAsStream(resourcePath);
    InputStreamReader streamReader = new InputStreamReader(inputStream, StandardCharsets.UTF_8);
    BufferedReader reader = new BufferedReader(streamReader);

    try {
      for (String line; (line = reader.readLine()) != null; ) {
        callback.op(line);
      }
    } catch (Exception ex) {
      throw new RuntimeException("Failed to read resource at path '" + resourcePath + "'.", ex);
    } finally {
      reader.close();
    }
  }
}
