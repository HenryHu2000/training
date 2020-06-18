package org.skygreen.training;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

class Permutations {

  private static void singlePermutationsHelper(String s, int index, boolean[] used, char[] output,
      Set<String> outputs) {
    if (index == s.length()) {
      outputs.add(String.valueOf(output));
      return;
    }
    for (int i = 0; i < used.length; i++) {
      if (!used[i]) {
        used[i] = true;
        output[index] = s.charAt(i);
        singlePermutationsHelper(s, index + 1, used, output, outputs);
        used[i] = false;
      }
    }
  }

  public static List<String> singlePermutations(String s) {
    // Your code here!
    Set<String> outputs = new HashSet<>();
    char[] output = new char[s.length()];
    boolean[] used = new boolean[s.length()];
    singlePermutationsHelper(s, 0, used, output, outputs);
    return new ArrayList<String>(outputs);
  }
}
