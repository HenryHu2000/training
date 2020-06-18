package org.skygreen.training;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.function.BiFunction;
import java.util.regex.Pattern;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

public class Calculator {
  private int jumpToIndex = 0;

  public static Double evaluate(String expression) {
    try {
      return Double.valueOf(
          new ScriptEngineManager().getEngineByName("JavaScript").eval(expression).toString());
    } catch (NumberFormatException | ScriptException e) {
      e.printStackTrace();
    }
    return 0.0;
    // return new Calculator().calculate(expression);
  }

  public double calculate(String expression) {

    List<Token> tokenList = tokenize(expression);
    jumpToIndex = 0;
    var expr = parse(tokenList, 0);
    return expr.evaluate();
  }

  private Expression parse(List<Token> tokenList, int from) {
    Stack<Expression> operandStack = new Stack<>();
    Stack<Operator> operatorStack = new Stack<>();

    first: for (int i = from; i < tokenList.size(); i++) {
      Token token = tokenList.get(i);
      if (token.isNumber) {
        double value = Double.parseDouble(token.string);
        operandStack.push(new NumericalExpression(value));
      } else {
        Operator currOp = null;
        switch (token.string.charAt(0)) {
          case '(':
            operandStack.push(parse(tokenList, i + 1));
            i = jumpToIndex;
            continue;
          case ')':
            jumpToIndex = i;
            break first;
          case '+':
            currOp = Operator.PLUS;
            break;
          case '-':
            currOp = Operator.MINUS;
            break;
          case '*':
            currOp = Operator.STAR;
            break;
          case '/':
            currOp = Operator.SLASH;
            break;
          case '~':
            currOp = Operator.TILDE;
            break;
          default:
            break;
        }
        if (operatorStack.isEmpty()) {
          operatorStack.push(currOp);
          continue;
        }

        Operator topOp = operatorStack.peek();
        while (topOp.precedence >= currOp.precedence) {
          pushToOperandStack(operandStack, operatorStack);
          if (operatorStack.isEmpty()) {
            break;
          }
          topOp = operatorStack.peek();
        }
        operatorStack.push(currOp);
      }
    }

    while (!operatorStack.isEmpty()) {
      pushToOperandStack(operandStack, operatorStack);
    }
    return operandStack.peek();
  }

  private static void pushToOperandStack(Stack<Expression> operandStack,
      Stack<Operator> operatorStack) {
    Expression exp2 = operandStack.pop();
    Expression exp1 = operandStack.pop();
    operandStack.push(new BinaryExpression(exp1, operatorStack.pop(), exp2));
  }

  private static List<Token> tokenize(String expression) {
    var remainingExpr = expression;
    List<Token> tokenList = new ArrayList<>();
    // int currIndex = 0;
    var numberPattern = Pattern.compile("^\\s*(\\-?\\d+\\.?\\d*)\\s*");
    var symbolPattern = Pattern.compile("^\\s*([+\\-*/()])\\s*");
    while (remainingExpr.length() > 0) {
      boolean isNumber = false;
      var matcher = numberPattern.matcher(remainingExpr);
      if (matcher.find()) {
        isNumber = true;
        if (tokenList.size() > 0 && tokenList.get(tokenList.size() - 1).isNumber) {
          tokenList.add(new Token("+", false));
        }
      } else {
        matcher = symbolPattern.matcher(remainingExpr);
        matcher.find();
        isNumber = false;
        if (matcher.group(1).equals("(") && tokenList.size() > 0
            && tokenList.get(tokenList.size() - 1).string.equals("-")) {


          boolean replace = tokenList.size() == 1;
          if (!replace) {
            String str = tokenList.get(tokenList.size() - 2).string;
            replace = str.equals("+") || str.equals("-") || str.equals("*") || str.equals("/")
                || str.equals("(");
          }
          if (replace) {
            tokenList.set(tokenList.size() - 1, new Token("~", false));
            tokenList.add(tokenList.size() - 1, new Token("0", true));
          }
        }
      }
      var str = matcher.group(1);
      tokenList.add(new Token(str, isNumber));
      remainingExpr = remainingExpr.substring(matcher.group().length());


    }
    return tokenList;
  }

  private static class Token {
    String string;
    boolean isNumber;

    Token(String string, boolean isNumber) {
      this.string = string;
      this.isNumber = isNumber;
    }

    @Override
    public String toString() {
      return string;
    }
  }

  private enum Operator {
    PLUS((x, y) -> x + y, 0), MINUS((x, y) -> x - y, 0), STAR((x, y) -> x * y,
        1), SLASH((x, y) -> x / y, 1), TILDE((x, y) -> x - y, 2);

    final BiFunction<Double, Double, Double> function;
    final int precedence;

    Operator(BiFunction<Double, Double, Double> function, int precedence) {
      this.precedence = precedence;
      this.function = function;
    }

    @Override
    public String toString() {
      var str = "";
      switch (this) {
        case PLUS:
          str = "+";
          break;
        case MINUS:
          str = "-";
          break;
        case STAR:
          str = "*";
          break;
        case SLASH:
          str = "/";
          break;
        case TILDE:
          str = "-";
          break;
        default:
          break;
      }
      return str;
    }
  }

  private static interface Expression {
    double evaluate();
  }

  private static class NumericalExpression implements Expression {
    final double value;

    NumericalExpression(double value) {
      this.value = value;
    }

    @Override
    public double evaluate() {
      return value;
    }

    @Override
    public String toString() {
      return Double.toString(value);
    }
  }

  private static class BinaryExpression implements Expression {
    final Expression left;
    final Expression right;
    final Operator operator;

    BinaryExpression(Expression left, Operator operator, Expression right) {
      this.left = left;
      this.right = right;
      this.operator = operator;
    }

    @Override
    public double evaluate() {
      return operator.function.apply(left.evaluate(), right.evaluate());
    }

    @Override
    public String toString() {
      return "(" + left.toString() + " " + operator.toString() + " " + right.toString() + ")";
    }
  }
}
