# Python 3 code to evaluate reverse polish notation


# function to evaluate reverse polish notation
import sys


def evaluate(expression):
  # splitting expression at whitespaces
  expression = expression.split()

  # stack
  stack = []

  # iterating expression
  for ele in expression:

    # ele is a number
    if ele not in '/*+-':
      stack.append(int(ele))

    # ele is an operator
    else:
      # getting operands
      right = stack.pop()
      left = stack.pop()

      # performing operation according to operator
      if ele == '+':
        stack.append(left + right)

      elif ele == '-':
        stack.append(left - right)

      elif ele == '*':
        stack.append(left * right)

      elif ele == '/':
        stack.append(int(left / right))

  # return final answer.
  return stack.pop()

# input expression
arr = sys.argv[1]

# calling evaluate()
answer = evaluate(arr)
# printing final value of the expression
print(f"Value of given expression'{arr}' = {answer}")
