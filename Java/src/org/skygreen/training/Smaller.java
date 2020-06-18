package org.skygreen.training;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Random;

public class Smaller {
  private static int binarySearch(List<Integer> list, int num, int from, int to) {
    if (from == to) {
      return from;
    }
    var mid = (from + to) / 2;
    if (num <= list.get(mid)) {
      return binarySearch(list, num, from, mid);
    } else {
      return binarySearch(list, num, mid + 1, to);
    }
  }

  private static int insertToList(List<Integer> list, int num) {
    var index = binarySearch(list, num, 0, list.size());
    list.add(index, num);
    return index;
  }

  public static int[] smaller(int[] arr) {

    var length = arr.length;
    BST sortedTree = new BST();
    var output = new int[arr.length];
    for (var i = length - 1; i >= 0; i--) {
      var num = arr[i];
      var index = sortedTree.insert(num);
      output[i] = index;
    }
    return output;
  }

  public static void main(String[] args) {

    // System.out.println(Arrays.compare(
    // smaller(new int[] {971, 731, 402, 728, -691, -697, 139, -268, 292, 419, 342, 47, -519, 573,
    // 102, 174, 155, -701, -422, -582, -34, -665, 655, 780, 448, 16, 837, -827, 509, -975,
    // -201, 806, 271, -193, 54, -762, 949, 197, -457, 229, 172, 607, 176, 133, -195, -694,
    // 843, 63, 195, -241, 427, 782, 12, 339, -469, -579, 0, 162, -398, 565, -689, 322, 275,
    // 25, -20, 905, 335, 714, 828, -212, -879, 544, -318, 707, -438, -679, -136, 789, 398,
    // 1000, 930, 864, -173, -688, -295, -325, -780, 538, -303, -986, 129, -534, 489, -17,
    // -332}),
    // new int[] {93, 81, 66, 79, 9, 7, 47, 26, 56, 61, 59, 40, 15, 65, 41, 46, 43, 6, 17, 11, 29,
    // 10, 58, 60, 51, 32, 62, 3, 50, 1, 21, 56, 40, 22, 29, 3, 57, 35, 10, 34, 31, 43, 31, 29,
    // 19, 3, 44, 25, 27, 16, 31, 37, 22, 28, 8, 6, 19, 21, 8, 27, 3, 20, 19, 17, 15, 27, 17,
    // 22, 23, 12, 1, 18, 8, 17, 5, 3, 9, 14, 11, 15, 14, 13, 8, 2, 6, 4, 1, 7, 3, 0, 3, 0, 2,
    // 1, 0}));
    //
    // System.out.println(Arrays.compare(
    // smaller(new int[] {-454, -976, 599, -854, 147, -795, 63, 886, 511, 257, 592, -195, -910,
    // -186, -219, 312, 228, 702, 424, -673, -681, 711, -969, -483, -56, -744, 468, 437, 99,
    // 470, -640, -647, -446, -111, 935, 418, -568, 779, 310, -139, 586, 350, -8, 387, 716,
    // -940, 903, 179, -841, 736, -861, -950, -84, -514, -784, 588, 801, 119, -485, -674, -939,
    // -886, -311, 292, -263, 514, 940, 759, 611, -331, -194, 811, 638, 175, -177, 585, -285,
    // 358, 85, 562, -369, -560, 320, 295, 549, 808, 709, 435, -424, 794, 942, -238, 306, 963,
    // -984, 512, -39, -529, 417}),
    // new int[] {25, 1, 77, 8, 46, 9, 41, 86, 64, 47, 70, 31, 5, 31, 29, 47, 42, 66, 52, 12, 10,
    // 64, 1, 17, 30, 8, 48, 47, 32, 46, 10, 9, 14, 24, 61, 40, 9, 53, 33, 22, 43, 33, 24, 33,
    // 44, 2, 49, 26, 5, 41, 4, 1, 18, 7, 3, 32, 37, 18, 6, 3, 1, 1, 6, 14, 7, 20, 30, 26, 23,
    // 5, 7, 25, 21, 10, 7, 18, 5, 11, 7, 14, 4, 1, 7, 5, 9, 11, 9, 7, 2, 7, 7, 2, 3, 5, 0, 3,
    // 1, 0, 0}));

    Random rand = new Random(1);
    int arrLength = 10000000;
    int[] arr = new int[arrLength];
    Arrays.setAll(arr, x -> rand.nextInt());
    int[] output = smaller(arr);
    System.out.println("Finished.");
  }

  private static class BST {
    Node root;

    int insert(int num, Node subtree) {
      assert (subtree != null);
      int index = 0;
      subtree.elementNum++;

      if (num <= subtree.value) {
        if (subtree.left == null) {
          subtree.left = new Node(num);
          index = 0;
        } else {
          index = insert(num, subtree.left);
        }
      } else {
        int indexInRight;
        if (subtree.right == null) {
          subtree.right = new Node(num);
          indexInRight = 0;
        } else {
          indexInRight = insert(num, subtree.right);
        }
        int leftSize = 0;
        if (subtree.left != null) {
          leftSize = subtree.left.elementNum;
        }
        index = leftSize + 1 + indexInRight;
      }
      return index;
    }

    int insert(int num) {
      int index;
      if (root != null) {
        index = insert(num, root);
      } else {
        index = 0;
        root = new Node(num);
      }
      return index;
    }

    private String nodeToString(Node node, int level) {
      String indentation = "";
      for (int i = 0; i < level; i++) {
        indentation += " ";
      }
      String output = indentation + node.toString() + "\n";

      output += node.left != null ? nodeToString(node.left, level + 1) : indentation + " NULL\n";
      output += node.right != null ? nodeToString(node.right, level + 1) : indentation + " NULL\n";

      return output;
    }

    @Override
    public String toString() {
      return root != null ? nodeToString(root, 0) : "";
    }

    private static class Node {
      int value;
      int elementNum;
      Node left;
      Node right;

      Node(int value) {
        this.value = value;
        this.elementNum = 1;
      }

      @Override
      public String toString() {
        return String.format("Value: %d, Number of nodes: %d", value, elementNum);
      }
    }
  }
}
