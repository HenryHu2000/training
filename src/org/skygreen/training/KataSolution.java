package org.skygreen.training;

import java.util.Arrays;

public class KataSolution {

  private static long factorialFrom(int n, int from) {
    assert (from >= 1);
    var arr = new long[n + 1];
    Arrays.setAll(arr, x -> x);
    return Arrays.stream(arr, from, n + 1).reduce(1, (x, y) -> x * y);
  }

  private static long combination(int n, int r) {
    return factorialFrom(n, n - r + 1) / factorialFrom(r, 1);
  }

  public static String expand(String expr) {
    var caretIndex = expr.indexOf('^');

    int n = Integer.parseInt(expr.substring(caretIndex + 1));
    if (n == 0) {
      return "1";
    }

    var baseStr = expr.substring(1, caretIndex - 1);

    var constantTermIndex = -1;
    for (int i = baseStr.length() - 1; i >= 0; i--) {
      if (!Character.isDigit(baseStr.charAt(i))) {
        constantTermIndex = i;
        break;
      }
    }
    assert (constantTermIndex != -1);
    int b = Integer.parseInt(baseStr.substring(constantTermIndex));
    char variable = baseStr.charAt(constantTermIndex - 1);
    var aStr = baseStr.substring(0, constantTermIndex - 1);
    if (aStr.length() == 0 || (aStr.length() == 1 && aStr.charAt(0) == '-')) {
      aStr += "1";
    }
    int a = Integer.parseInt(aStr);

    StringBuilder builder = new StringBuilder();

    boolean first = true;
    for (int i = 0; i <= n; i++) {
      String termStr = "";
      long coefficient = combination(n, i) * (long) Math.pow(a, n - i) * (long) Math.pow(b, i);
      if (coefficient != 0) {
        String coefficientStr = Long.toString(coefficient);
        if (n - i > 0) {
          if (coefficient == 1) {
            coefficientStr = "";
          } else if (coefficient == -1) {
            coefficientStr = "-";
          }
        }
        if (first) {
          termStr += coefficientStr;
        } else {
          termStr += coefficient > 0 ? "+" + coefficientStr : coefficientStr;
        }

        if (n - i > 0) {
          termStr += variable;
          if (n - i > 1) {
            termStr += "^" + (n - i);
          }
        }
        builder.append(termStr);
        first = false;
      }
    }
    return builder.toString();
  }
}
