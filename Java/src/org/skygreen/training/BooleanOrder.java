package org.skygreen.training;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;

public class BooleanOrder {
  private boolean[] operandArr;
  private OperatorType[] operatorArr;
  private Map<String, Count> map;

  public BooleanOrder(final String operands, final String operators) {
    operandArr = new boolean[operands.length()];
    operatorArr = new OperatorType[operands.length()];
    map = new HashMap<>();

    for (int i = 0; i < operators.length(); i++) {
      operandArr[i] = operands.charAt(i) == 't';
      switch (operators.charAt(i)) {
        case '&':
          operatorArr[i] = OperatorType.AND;
          break;
        case '|':
          operatorArr[i] = OperatorType.OR;
          break;
        case '^':
          operatorArr[i] = OperatorType.XOR;
          break;
        default:
          break;
      }
    }
    operandArr[operands.length() - 1] = operands.charAt(operands.length() - 1) == 't';

  }

  private String positionsToString(int from, int to) {
    StringBuilder builder = new StringBuilder();
    for (int i = from; i < to; i++) {
      if (operandArr[i]) {
        builder.append('t');
      } else {
        builder.append('f');
      }
      switch (operatorArr[i]) {
        case AND:
          builder.append('&');
          break;
        case OR:
          builder.append('|');
          break;
        case XOR:
          builder.append('^');
          break;
        default:
          break;
      }
    }
    if (operandArr[to]) {
      builder.append('t');
    } else {
      builder.append('f');
    }

    return builder.toString();
  }

  private Count solveHelper(int from, int to) {
    if (from == to) {
      var count = new Count(BigInteger.ONE, operandArr[from] ? BigInteger.ONE : BigInteger.ZERO);
      return count;
    }

    var str = positionsToString(from, to);
    if (map.containsKey(str)) {
      return map.get(str);
    }

    var sumCount = new Count(BigInteger.ZERO, BigInteger.ZERO);
    for (int i = from; i < to; i++) {

      var leftCount = solveHelper(from, i);
      var rightCount = solveHelper(i + 1, to);
      Count currCount = null;
      switch (operatorArr[i]) {
        case AND:
          currCount = leftCount.and(rightCount);
          break;
        case OR:
          currCount = leftCount.or(rightCount);
          break;
        case XOR:
          currCount = leftCount.xor(rightCount);
          break;
        default:
          break;
      }
      sumCount = sumCount.add(currCount);
    }

    map.put(str, sumCount);

    return sumCount;
  }

  public BigInteger solve() {
    return solveHelper(0, operandArr.length - 1).trueCount;
  }

  private enum OperatorType {
    AND, OR, XOR
  }

  private static class Count {
    final BigInteger totalCount;
    final BigInteger trueCount;

    Count(BigInteger totalCount, BigInteger trueCount) {
      this.totalCount = totalCount;
      this.trueCount = trueCount;
    }

    Count add(Count aCount) {
      return new Count(totalCount.add(aCount.totalCount), trueCount.add(aCount.trueCount));
    }

    Count and(Count aCount) {
      return new Count(totalCount.multiply(aCount.totalCount),
          trueCount.multiply(aCount.trueCount));
    }

    Count or(Count aCount) {
      var newTotalCount = totalCount.multiply(aCount.totalCount);
      return new Count(newTotalCount, newTotalCount.subtract(
          totalCount.subtract(trueCount).multiply(aCount.totalCount.subtract(aCount.trueCount))));

    }

    Count xor(Count aCount) {
      return new Count(totalCount.multiply(aCount.totalCount),
          trueCount.multiply(aCount.totalCount.subtract(aCount.trueCount))
              .add(totalCount.subtract(trueCount).multiply(aCount.trueCount)));
    }
  }
}
