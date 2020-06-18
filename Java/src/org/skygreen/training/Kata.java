package org.skygreen.training;

import java.util.Arrays;
import java.util.stream.Collectors;

public class Kata {

  public static long nextBiggerNumber(long n) {
    int zeroChar = (new String("0")).chars().findFirst().getAsInt();
    var digitArr = Long.toString(n).chars().map(x -> x - zeroChar).toArray();

    for (int i = digitArr.length - 2; i >= 0; i--) {
      if (digitArr[i] < digitArr[i + 1]) {
        int index1ToSwap = i;
        int index2ToSwap = -1;
        for (int j = digitArr.length - 1; j >= i + 1; j--) {
          if (digitArr[j] > digitArr[index1ToSwap]) {
            index2ToSwap = j;
            break;
          }
        }
        assert (index2ToSwap != -1);
        int digitToSwap = digitArr[index1ToSwap];
        digitArr[index1ToSwap] = digitArr[index2ToSwap];
        digitArr[index2ToSwap] = digitToSwap;

        var newSubArr = new int[digitArr.length - i - 1];
        for (int j = i + 1; j < digitArr.length; j++) {
          newSubArr[j - i - 1] = digitArr[j];
        }
        Arrays.sort(newSubArr);
        for (int j = i + 1; j < digitArr.length; j++) {
          digitArr[j] = newSubArr[j - i - 1];
        }

        var output = Long.parseLong(
            new String(Arrays.stream(digitArr).map(x -> Integer.toString(x).charAt(0)).toArray(), 0,
                digitArr.length));
        return output;
      }
    }
    return -1;
  }

  public static long nextSmaller(long n) {
    int zeroChar = (new String("0")).chars().findFirst().getAsInt();
    var digitArr = Long.toString(n).chars().map(x -> x - zeroChar).toArray();

    for (int i = digitArr.length - 2; i >= 0; i--) {
      if (digitArr[i] > digitArr[i + 1]) {
        int index1ToSwap = i;
        int index2ToSwap = -1;
        for (int j = digitArr.length - 1; j >= i + 1; j--) {
          if (digitArr[j] < digitArr[index1ToSwap]) {
            index2ToSwap = j;
            break;
          }
        }
        assert (index2ToSwap != -1);
        int digitToSwap = digitArr[index1ToSwap];
        digitArr[index1ToSwap] = digitArr[index2ToSwap];
        digitArr[index2ToSwap] = digitToSwap;

        var newSubArr = new int[digitArr.length - i - 1];
        for (int j = i + 1; j < digitArr.length; j++) {
          newSubArr[j - i - 1] = digitArr[j];
        }
        Arrays.sort(newSubArr);
        for (int j = i + 1; j < digitArr.length; j++) {
          digitArr[j] = newSubArr[newSubArr.length - (j - i - 1) - 1];
        }

        var output = Long.parseLong(
            new String(Arrays.stream(digitArr).map(x -> Integer.toString(x).charAt(0)).toArray(), 0,
                digitArr.length));
        if (Long.toString(output).length() < digitArr.length) {
          return -1;
        }
        return output;
      }
    }
    return -1;
  }
}
