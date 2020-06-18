package org.skygreen.training;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class BalancedParens {
  private static void balancedParensHelper(int n, List<String>[] lists) {
    assert (n > 0);
    if (n == 1) {
      lists[1] = Arrays.asList("()");
      return;
    }

    balancedParensHelper(n - 1, lists);

    Set<String> set = new HashSet<>();
    for (int i = 1; i < n; i++) {
      for (String str1 : lists[i]) {
        for (String str2 : lists[n - i]) {
          set.add(str1 + str2);
        }
      }
    }
    for (String str : lists[n - 1]) {
      // set.add("()" + str);
      set.add("(" + str + ")");
      // set.add(str + "()");
    }
    lists[n] = new ArrayList<>(set);
  }

  public static List<String> balancedParens(int n) {
    // your code here
    if (n == 0) {
      return Arrays.asList("");
    }
    List<String>[] lists = new List[n + 1];
    balancedParensHelper(n, lists);
    return lists[n];
  }
}
