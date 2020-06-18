package org.skygreen.training;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class RectanglesUnion {

  private static boolean inRectangle(int[] rectangle, Map<Integer, Integer> xIndexes, int x,
      Map<Integer, Integer> yIndexes, int y) {
    boolean inX = x >= xIndexes.get(rectangle[0]) && x < xIndexes.get(rectangle[2]);
    boolean inY = y >= yIndexes.get(rectangle[1]) && y < yIndexes.get(rectangle[3]);

    return inX && inY;
  }

  private static Integer[] discretize(Integer[] arr) {
    var set = new TreeSet<Integer>();
    set.addAll(Arrays.asList(arr));
    return set.toArray(new Integer[0]);
  }

  public static int calculateSpace(int[][] rectangles) {
    if (rectangles.length == 0) {
      return 0;
    }

    Integer[] xs = discretize(Stream
        .concat(Arrays.stream(rectangles).map(rec -> rec[0]),
            Arrays.stream(rectangles).map(rec -> rec[2]))
        .collect(Collectors.toList()).toArray(new Integer[0]));
    Integer[] ys = discretize(Stream
        .concat(Arrays.stream(rectangles).map(rec -> rec[1]),
            Arrays.stream(rectangles).map(rec -> rec[3]))
        .collect(Collectors.toList()).toArray(new Integer[0]));

    Map<Integer, Integer> xIndexes = new HashMap<>();
    for (int i = 0; i < xs.length; i++) {
      xIndexes.put(xs[i], i);
    }
    Map<Integer, Integer> yIndexes = new HashMap<>();
    for (int i = 0; i < ys.length; i++) {
      yIndexes.put(ys[i], i);
    }

    var tree = new SegmentTree(xs.length);
    double[] lengths = new double[xs.length - 1];
    Arrays.setAll(lengths, i -> xs[i + 1] - xs[i]);
    tree.setLengths(lengths);

    Set<RectangleSide> sides = new TreeSet<>();
    for (int[] rec : rectangles) {
      if (rec[2] - rec[0] > 0) {
        sides.add(new RectangleSide(rec[1], rec[0], rec[2], true));
        sides.add(new RectangleSide(rec[3], rec[0], rec[2], false));
      }
    }

    int areaSum = 0;
    RectangleSide[] sideList = sides.toArray(new RectangleSide[0]);
    for (int i = 0; i < sideList.length - 1; i++) {
      if (sideList[i].entering) {
        tree.addSegment(xIndexes.get(sideList[i].x1), xIndexes.get(sideList[i].x2));
      } else {
        tree.removeSegment(xIndexes.get(sideList[i].x1), xIndexes.get(sideList[i].x2));
      }
      areaSum += tree.getActiveLengthSum(0, xs.length - 1) * (sideList[i + 1].y - sideList[i].y);
    }

    // for (int i = 0; i < xs.length - 1; i++) {
    // for (int j = 0; j < ys.length - 1; j++) {
    // for (int k = 0; k < rectangles.length; k++) {
    // if (inRectangle(rectangles[k], xIndexes, i, yIndexes, j)) {
    // areaSum += (xs[i + 1] - xs[i]) * (ys[i + 1] - ys[i]);
    // break;
    // }
    // }
    // }
    // }

    return areaSum;
  }

  private static class RectangleSide implements Comparable<RectangleSide> {
    final int y;
    final int x1;
    final int x2;
    final boolean entering;

    RectangleSide(int y, int x1, int x2, boolean entering) {
      this.y = y;
      this.x1 = x1;
      this.x2 = x2;
      this.entering = entering;
    }

    @Override
    public int compareTo(RectangleSide aRectangleSide) {
      int output = Integer.compare(this.y, aRectangleSide.y);
      return output != 0 ? output : Integer.compare(this.hashCode(), aRectangleSide.hashCode());
    }
  }

  private static class SegmentTree {
    private SegmentNode root;

    SegmentTree(int nodeNum) {
      root = createSubtree(0, nodeNum - 1);
    }

    private SegmentNode createSubtree(int from, int to) {
      assert (to - from > 0);

      var subtree = new SegmentNode(from, to);
      if (to - from == 1) {
        return subtree;
      }

      int mid = (from + to) / 2;
      subtree.left = createSubtree(from, mid);
      subtree.right = createSubtree(mid, to);

      return subtree;
    }

    private double getActiveLengthSumHelper(int from, int to, SegmentNode subtree) {
      int treeFrom = subtree.from;
      int treeTo = subtree.to;
      assert (from >= treeFrom);
      assert (to <= treeTo);
      assert (to > from);

      if (treeTo - treeFrom == 1) {
        return subtree.count > 0 ? subtree.length : 0;
      }

      if (treeFrom == from && treeTo == to) {
        if (subtree.count > 0) {
          return subtree.length;
        }
      }
      int treeMid = subtree.left.to;
      if (to <= treeMid) {
        return getActiveLengthSumHelper(from, to, subtree.left);
      } else if (from >= treeMid) {
        return getActiveLengthSumHelper(from, to, subtree.right);
      } else {
        return getActiveLengthSumHelper(from, treeMid, subtree.left)
            + getActiveLengthSumHelper(treeMid, to, subtree.right);
      }
    }

    public double getActiveLengthSum(int from, int to) {
      return getActiveLengthSumHelper(from, to, root);
    }

    private double getLengthSumHelper(int from, int to, SegmentNode subtree) {
      int treeFrom = subtree.from;
      int treeTo = subtree.to;
      assert (from >= treeFrom);
      assert (to <= treeTo);
      assert (to > from);

      if (treeFrom == from && treeTo == to) {
        return subtree.length;
      }
      int treeMid = subtree.left.to;
      if (to <= treeMid) {
        return getLengthSumHelper(from, to, subtree.left);
      } else if (from >= treeMid) {
        return getLengthSumHelper(from, to, subtree.right);
      } else {
        return getLengthSumHelper(from, treeMid, subtree.left)
            + getLengthSumHelper(treeMid, to, subtree.right);
      }
    }

    public double getLengthSum(int from, int to) {
      return getLengthSumHelper(from, to, root);
    }

    private void setSubtreeLengths(double[] lengths, SegmentNode subtree) {
      if (subtree.to - subtree.from == 1) {
        subtree.length = lengths[subtree.from];
        return;
      }
      setSubtreeLengths(lengths, subtree.left);
      setSubtreeLengths(lengths, subtree.right);
      subtree.length = subtree.left.length + subtree.right.length;
    }

    public void setLengths(double[] lengths) {
      assert (lengths.length == getNodeNumber() - 1);
      setSubtreeLengths(lengths, root);
    }

    private void changeSubtreeCount(int from, int to, SegmentNode subtree, int change) {
      int treeFrom = subtree.from;
      int treeTo = subtree.to;
      assert (from >= treeFrom);
      assert (to <= treeTo);
      assert (to > from);

      if (from == treeFrom && to == treeTo) {
        subtree.count += change;
        assert (subtree.count >= 0);

        var left = subtree.left;
        if (left != null) {
          changeSubtreeCount(from, left.to, left, change);
        }
        var right = subtree.right;
        if (right != null) {
          changeSubtreeCount(right.from, to, right, change);
        }
        return;
      }
      int treeMid = subtree.left.to;
      if (to <= treeMid) {
        changeSubtreeCount(from, to, subtree.left, change);
      } else if (from >= treeMid) {
        changeSubtreeCount(from, to, subtree.right, change);
      } else {
        changeSubtreeCount(from, treeMid, subtree.left, change);
        changeSubtreeCount(treeMid, to, subtree.right, change);
      }
    }

    public void addSegment(int from, int to) {
      changeSubtreeCount(from, to, root, 1);
    }

    public void removeSegment(int from, int to) {
      changeSubtreeCount(from, to, root, -1);
    }

    public int getNodeNumber() {
      return root.to + 1;
    }

    private String nodeToString(SegmentNode node, int level) {
      String indentation = "";
      for (int i = 0; i < level; i++) {
        indentation += " ";
      }
      String output = indentation + node.toString() + "\n";
      if (node.left != null && node.right != null) {
        return output + nodeToString(node.left, level + 1) + nodeToString(node.right, level + 1);
      }
      return output;
    }

    @Override
    public String toString() {
      return nodeToString(root, 0);
    }

    private class SegmentNode {
      final int from;
      final int to;
      SegmentNode left;
      SegmentNode right;
      int count;
      double length;

      SegmentNode(int from, int to) {
        this.from = from;
        this.to = to;
      }

      @Override
      public String toString() {
        return String.format("[%d, %d] Count: %d, Length: %f", from, to, count, length);
      }
    }
  }
}
