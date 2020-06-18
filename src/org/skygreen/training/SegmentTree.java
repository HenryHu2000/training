package org.skygreen.training;

public class SegmentTree {
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
