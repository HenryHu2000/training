function insertHelper(num, subtree) {
  assert (subtree != null);
  let index = 0;
  subtree.elementNum++;

  if (num <= subtree.value) {
    if (subtree.left == null) {
      subtree.left = new Node(num);
      index = 0;
    } else {
      index = insertHelper(num, subtree.left);
    }
  } else {
    let indexInRight;
    if (subtree.right == null) {
      subtree.right = new Node(num);
      indexInRight = 0;
    } else {
      indexInRight = insertHelper(num, subtree.right);
    }
    let leftSize = 0;
    if (subtree.left != null) {
      leftSize = subtree.left.elementNum;
    }
    index = leftSize + 1 + indexInRight;
  }
  return index;
}

class BST {
  constructor() {
    root = null;
  }

  insert(num) {
    let index;
    if (root != null) {
      index = insertHelper(num, root);
    } else {
      index = 0;
      root = new Node(num);
    }
    return index;
  }
}

class Node {
  constructor(value) {
    this.value = value;
    this.elementNum = 1;
    this.left = null;
    this.right = null;
  }
}

function smaller(arr) {
  let length = arr.length;
  let sortedTree = new BST();
  let output = [];
  for (let i = length - 1; i >= 0; i--) {
    let num = arr[i];
    let index = sortedTree.insert(num);
    output[i] = index;
    
    if (i % 1000 == 0) {
      console.log(i + " left.");
    }
  }
  return output;
}

