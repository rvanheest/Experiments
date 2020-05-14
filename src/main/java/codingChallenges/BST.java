package codingChallenges;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class BST {

  private Node root = null;

  public static BST empty() {
    return new BST();
  }

  public static BST from(int... values) {
    BST bst = new BST();
    for (int value : values) {
      bst.add(value);
    }
    return bst;
  }

  public boolean isEmpty() {
    return root == null;
  }

  public void add(int element) {
    if (isEmpty())
      root = new Node(element);
    else
      root.add(element);
  }

  public boolean contains(int element) {
    return !isEmpty() && root.contains(element);
  }

  public Stream<Integer> traverseDeptFirstPreOrder() {
    if (isEmpty())
      return Stream.empty();
    else
      return root.traverseDeptFirstPreOrder();
  }

  public Stream<Integer> traverseDeptFirstInOrder() {
    if (isEmpty())
      return Stream.empty();
    else
      return root.traverseDeptFirstInOrder();
  }

  public Stream<Integer> traverseDeptFirstPostOrder() {
    if (isEmpty())
      return Stream.empty();
    else
      return root.traverseDeptFirstPostOrder();
  }

  public Stream<Integer> traverseBreadthFirst() {
    if (isEmpty())
      return Stream.empty();
    else
      return root.traverseBreadthFirst();
  }

  @Override public String toString() {
    if (isEmpty())
      return "Empty BST";
    else
      return String.valueOf(root);
  }

  private static class Node {

    private final int value;
    private Node left;
    private Node right;

    public Node(int value, Node left, Node right) {
      this.value = value;
      this.left = left;
      this.right = right;
    }

    public Node(int value) {
      this(value, null, null);
    }

    public void add(int element) {
      if (element <= value) {
        if (left == null)
          left = new Node(element);
        else
          left.add(element);
      }
      else {
        if (right == null)
          right = new Node(element);
        else
          right.add(element);
      }
    }

    public boolean contains(int element) {
      return containsTR(this, element);
    }

    private static boolean containsTR(Node node, int element) {
      if (element == node.value)
        return true;
      else if (element < node.value)
        return node.left != null && containsTR(node.left, element);
      else
        return node.right != null && containsTR(node.right, element);
    }

    public Stream<Integer> traverseDeptFirstPreOrder() {
      Stream<Integer> head = Stream.of(value);
      Stream<Integer> tail = Stream.of(left, right)
          .filter(bst -> bst != null)
          .flatMap(Node::traverseDeptFirstPreOrder);

      return Stream.concat(head, tail);
    }

    public Stream<Integer> traverseDeptFirstInOrder() {
      Stream<Integer> head = Stream.of(value);
      Stream<Integer> l = Stream.of(left)
          .filter(bst -> bst != null)
          .flatMap(Node::traverseDeptFirstInOrder);
      Stream<Integer> r = Stream.of(right)
          .filter(bst -> bst != null)
          .flatMap(Node::traverseDeptFirstInOrder);

      return Stream.concat(l, Stream.concat(head, r));
    }

    public Stream<Integer> traverseDeptFirstPostOrder() {
      Stream<Integer> head = Stream.of(value);
      Stream<Integer> l = Stream.of(left)
          .filter(bst -> bst != null)
          .flatMap(Node::traverseDeptFirstPostOrder);
      Stream<Integer> r = Stream.of(right)
          .filter(bst -> bst != null)
          .flatMap(Node::traverseDeptFirstPostOrder);

      return Stream.concat(l, Stream.concat(r, head));
    }

    public Stream<Integer> traverseBreadthFirst() {
      return tbf(Collections.singletonList(this));
    }

    private static Stream<Integer> tbf(List<Node> bsts) {
      if (bsts.isEmpty())
        return Stream.empty();
      else {
        Stream<Integer> thisRow = bsts.stream().map(bst -> bst.value);
        List<Node> below = bsts.stream()
            .flatMap(bst -> Stream.of(bst.left, bst.right).filter(b -> b != null))
            .collect(Collectors.toList());

        return Stream.concat(thisRow, tbf(below));
      }
    }

    @Override public String toString() {
      return "Node(" + String.valueOf(value) + ", " + String.valueOf(left) + ", " + String.valueOf(right) + ")";
    }
  }
}

class BSTTest {
  public static void main(String[] args) {
  	BST tree = BST.from(5, 3, 7, 1, 2, 4, 6, 8);

    /*
                    5
            3               7
        1       4       6       8
      -   2   -   -   -   -   -   -
     */

    System.out.println(tree.toString());

    System.out.println(tree.contains(2));
    System.out.println(tree.contains(0));
    System.out.println(tree.contains(10));

    System.out.println(tree.traverseDeptFirstPreOrder().collect(Collectors.toList()));
    System.out.println(tree.traverseDeptFirstInOrder().collect(Collectors.toList()));
    System.out.println(tree.traverseDeptFirstPostOrder().collect(Collectors.toList()));
    System.out.println(tree.traverseBreadthFirst().collect(Collectors.toList()));
  }
}
