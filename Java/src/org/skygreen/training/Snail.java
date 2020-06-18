package org.skygreen.training;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Snail {

  public static void traverseBorder(int[][] array, int size, List<Integer> list) {
    var n = array.length;
    var margin = (n - size) / 2;
    if (size == 1) {
      list.add(array[margin][margin]);
      return;
    }
    for (int j = margin; j < n - margin - 1; j++) {
      list.add(array[margin][j]);
    }
    for (int i = margin; i < n - margin - 1; i++) {
      list.add(array[i][n - margin - 1]);
    }
    for (int j = n - margin - 1; j >= margin + 1; j--) {
      list.add(array[n - margin - 1][j]);
    }
    for (int i = n - margin - 1; i >= margin + 1; i--) {
      list.add(array[i][margin]);
    }
  }

  public static int[] snail(int[][] array) {
    // enjoy
    var n = array.length;
    if (array[0].length == 0) {
      return new int[] {};
    }

    var list = new ArrayList<Integer>();
    for (int i = n; i >= 1; i -= 2) {
      traverseBorder(array, i, list);
    }
    return list.stream().mapToInt(x -> x).toArray();
  }

  public static void main(String[] args) {
    var arr = new int[][] {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
    System.out.println(Arrays.toString(snail(arr)));
  }
}
