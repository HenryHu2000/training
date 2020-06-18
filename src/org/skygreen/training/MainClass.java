package org.skygreen.training;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.regex.Pattern;

public class MainClass {

  public static void main(String[] args) {
    // var str = "-1245.343 34 56";
    // var str = ")";
    // var numberMatcher = Pattern.compile("^\\s*(\\-?\\d+\\.?\\d*)\\s*").matcher(str);
    // var symbolMatcher = Pattern.compile("^\\s*([+\\-*/()])\\s*").matcher(str);
    //
    // symbolMatcher.find(0);
    // System.out.println(symbolMatcher.group(1));

    // var list = tokenize("(2 / (2 + 3.33) * 4) - -6");
    // list.forEach(x -> {
    // System.out.println(x.string);
    // });

    // var str = "2 / 2 + 3.33 * 4 - -6";

    // var scanner = new Scanner(System.in);
    // while (true) {
    // var str = scanner.nextLine();
    // var eval = new MathEvaluator();
    // var output = eval.calculate(str);
    // System.out.println(output);
    // }
    // var str = "(1 - 2) + -(-(-(-4)))";
    // var eval = new MathEvaluator();
    // var output = eval.calculate(str);
    // System.out.println(output);

    var arr = new int[] {0, 1, 3, 3, 3, 3, 6, 7, 8, 9};
    for (int i = 0; i < arr.length; i++) {
      var index = binarySearch(arr, i, 0, arr.length - 1);
      System.out.println(i + ": " + index);      
    }
  }

  private static int binarySearch(int[] arr, int num, int from, int to) {
    if (from == to) {
      return from;
    }
    var mid = (from + to) / 2;
    if (num <= arr[mid]) {
      return binarySearch(arr, num, from, mid);
    } else {
      return binarySearch(arr, num, mid + 1, to);
    }
  }

}
