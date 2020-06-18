package org.skygreen.training;

import java.util.Arrays;
import java.util.Random;

public class OldMainClass {

  public static void main(String[] args) {
    // var count = new BooleanOrder("ttftfftftffttfftftftfftft",
    // "|&^&&||^&&^^|&&||^&&||&^").solve();
    // System.out.println(count);

    // Random rand = new Random(1);
    // int[][] recs = {{0, 4, 11, 6}};
    // int[][] recs = {{5, 5, 15, 15}, {0, 0, 10, 10}};
    // Random rand = new Random(1);
    // int[][] recs = new int[100000][4];
    // int range = 10000;
    // for (int[] rec : recs) {
    // rec[0] = rand.nextInt(range);
    // rec[1] = rand.nextInt(range);
    // rec[2] = rec[0] + rand.nextInt(range);
    // rec[3] = rec[1] + rand.nextInt(range);
    // }
    // var area = RectanglesUnion.calculateSpace(recs);
    // System.out.println(area);


    // int nodeNum = 10000000;
    // var tree = new SegmentTree(nodeNum);
    // double[] lengths = new double[nodeNum];
    // Arrays.setAll(lengths, x -> x);
    // tree.setLengths(lengths);
    // System.out.println(tree.getLengthSum(0, 101));
    // int nodeNum = 10;
    // var tree = new SegmentTree(nodeNum);
    // double[] lengths = new double[nodeNum];
    // Arrays.setAll(lengths, x -> x);
    // tree.setLengths(lengths);
    // tree.addSegment(1, 6);
    // tree.addSegment(3, 7);
    // tree.addSegment(7, 9);
    // tree.removeSegment(1, 6);
    // tree.removeSegment(3, 7);
    // tree.addSegment(1, 3);
    // tree.addSegment(2, 3);
    // tree.addSegment(5, 8);
    // tree.addSegment(8, 9);
    // System.out.println(tree);
    // System.out.println(tree.getActiveLengthSum(0, nodeNum - 1));

    // var list = Permutations.singlePermutations("abcd");
    // System.out.println(list);

    // var list = BalancedParens.balancedParens(13);
    // System.out.println(list);

    // for (int i = 0; i < 10; i++) {
    // System.out.println(Character.getNumericValue(Integer.toString(i).charAt(0))
    // - Character.getNumericValue('0'));
    // }
    // System.out.println(Character.getNumericValue('0'));
    // int zeroChar = (new String("0")).chars().findFirst().getAsInt();
    // var digitArr = Long.toString(1234543210).chars().map(x -> x - zeroChar).toArray();
    // System.out.println(Arrays.toString(digitArr));
    // var output = Long.parseLong(
    // new String(Arrays.stream(digitArr).map(x -> Integer.toString(x).charAt(0)).toArray(), 0,
    // digitArr.length));
    // System.out.println(output);
    // Long output = Kata.nextBiggerNumber(98876);
    // System.out.println(output);

    var str = KataSolution.expand("(2x-3)^3");
    System.out.println(str);

  }

}
