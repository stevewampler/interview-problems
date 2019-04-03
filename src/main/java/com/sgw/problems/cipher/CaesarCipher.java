package com.sgw.problems.cipher;

import com.sgw.utils.IOUtils;

import java.util.HashMap;
import java.util.Map;

/**
 * A java implementation of the CaesarCipher and a method to decoded a Caesar Cipher'd string using quad frequencies.
 *
 * See http://practicalcryptography.com/cryptanalysis/stochastic-searching/cryptanalysis-caesar-cipher/
 */
public class CaesarCipher {
    public static void main(String[] args) {
        String original = "defend the east wall of the castle!";
        String ciphered = cipher(original, 25);
        String deciphered = cipher(ciphered, -25);
        String decoded = decode(ciphered);

        System.out.println(original);
        System.out.println(ciphered);
        System.out.println(deciphered);
        System.out.println(decoded);
    }

    public static String cipher(String str, int key) {
        String upperStr = str.toUpperCase();

        StringBuilder builder = new StringBuilder();

        for(int i = 0; i < upperStr.length(); i++) {
            int c = (int) upperStr.charAt(i);

            if (Character.isLetter(c)) {
                int nc = (c - 'A') + key;
                int nc2;

                if (nc < 0) {
                    nc2 = 26 + nc;
                } else if (nc > 25) {
                    nc2 = nc - 26;
                } else {
                    nc2 = nc;
                }

                builder.append((char) (nc2 + 'A'));
            }
        }

        return builder.toString();
    }

    private static Map<String, Double> quadgrams = new HashMap();
    private static long sumScores = 0L;
    private static double defaultScore;

    static {
        Map<String, Double> map = new HashMap();

        IOUtils.LineCallback callback = new IOUtils.LineCallback() {
            public void op(String line) {
                String[] splitLine = line.split(" ");

                String key = splitLine[0];
                Double value = Double.parseDouble(splitLine[1]);

                map.put(key, value);

                sumScores += value;
            }
        };

        try {
            IOUtils.readLinesFromResource("/english_quadgrams.txt", callback);
        } catch (Exception ex) {
            throw new RuntimeException("Failed to read quadgrams.", ex);
        }

//        try {
//            InputStream inputStream = CaesarCipher.class.getResourceAsStream("/english_quadgrams.txt");
//            InputStreamReader streamReader = new InputStreamReader(inputStream, StandardCharsets.UTF_8);
//            BufferedReader reader = new BufferedReader(streamReader);
//            for (String line; (line = reader.readLine()) != null; ) {
//                String[] splitLine = line.split(" ");
//
//                String key = splitLine[0];
//                Double value = Double.parseDouble(splitLine[1]);
//
//                map.put(key, value);
//
//                sumScores += value;
//            }
//        } catch (Exception ex) {
//            throw new RuntimeException("Failed to read quadgrams.", ex);
//        }

        for (String key: map.keySet()) {
            Double score = map.get(key);

            quadgrams.put(key, Math.log10(score / sumScores));
        }

        defaultScore = Math.log10(0.01/sumScores);
    }

    private static double score(String str) {
        int strLen = str.length();
        double totalScore = 0.0;

        for (int i = 0; i < strLen - 4; i++) {
          String quad = str.substring(i, i + 4);
          Double score = quadgrams.getOrDefault(quad, defaultScore);
          totalScore += score;
        }

        return totalScore;
    }

    public static String decode(String str) {
        // try all of the keys in 1-25
        double bestScore = Double.NEGATIVE_INFINITY;
        String bestString = "";

        for (int key = 1; key <= 25; key++) {
            String decipheredString = cipher(str, -key);

            double score = score(decipheredString);

            if (score > bestScore) {
                bestScore = score;
                bestString = decipheredString;
            }
        }

        return bestString;
    }
}
