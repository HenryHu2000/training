package org.skygreen.training;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

public class Fibonacci {
  static final int PRECISION = 1000;
  static final BigDecimal ONE = new BigDecimal(new char[] {'1'}, 0, 1);
  static final BigDecimal TWO = new BigDecimal(new char[] {'2'}, 0, 1);
  static final BigDecimal FIVE = new BigDecimal(new char[] {'5'}, 0, 1);

  public static BigInteger fib(BigInteger n) {
    MathContext context = new MathContext(PRECISION);
    final BigDecimal SQRT_FIVE = FIVE.sqrt(context);

    final BigDecimal PHI = ONE.add(SQRT_FIVE).divide(TWO);
    final BigDecimal PHI_PRIME = ONE.subtract(SQRT_FIVE).divide(TWO);

    BigDecimal output = PHI.pow(n.intValueExact()).subtract(PHI_PRIME.pow(n.intValueExact()))
        .divide(SQRT_FIVE, context);
    return output.round(MathContext.DECIMAL128).toBigInteger();
  }

  public static void main(String[] args) {
    for (int i = 0; i < 1000; i++) {
      System.out.println(fib(new BigInteger(Integer.toString(i), 10)));
    }

  }
}
