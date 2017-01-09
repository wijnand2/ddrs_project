#########
# Usage #
#########
# Call: python unfold.py [input_path] [output_path],
# input_path must be specified, output_path will default to input_path
# but with .recipe replaced with .trs. 'Notational devices' can be specified 
# at the start of the rules with [i, j]{n, k} syntax, where the symbols in the 
# brackets are the variables which each range from n to k. If these symbols 
# occur in function names they will also be replaced which can either produce 
# bugs or be used as a feature since functions can have numbers in them, and 
# so this allows for variable functions to occur in rules. In addition *, ', 
# and ` can be used as defined in Albans paper.

from itertools import product
import sys

# Matches opening bracket with closing bracket.
# Starts finding brackets at position n.
def match_brackets(term, n):
    opened = 0
    i  = term.find('(', n)
    for j in range(i+1, len(term)):
        if term[j] == '(':
            opened += 1
        elif term[j] == ')':
            if not opened:
                return (i, j)
            elif opened > 0:
                opened -= 1

    # Returns 'error' if bracket did not close.
    return ('error', 'error')

# Expands all occurances of '_{expression}' proceeding unary function
# calls with a number of calls equal to the expression. Example:
# >>> print(exp_funcs('P_0(P(S_1+1(3), S_0(4)))'))
# P(S(S(3)), 4)
def exp_funcs(rule):
    # Store intermediate results in temp.
    temp = rule

    # Loop until all occurances of '_' are gone, removing one each iteration.
    while(True):
        loc = temp.find('_')
        if loc == -1:
            break
        else:
            # Find indices of opening and closing brackets of argument.
            bo, bc = match_brackets(temp, loc)
            if bo == 'error':
                print('something went wrong with matching brackets')
                break

            # Find the name of the function, assuming the start is indicated
            # by a space, comma, opening bracket, or start of sentence.
            f_start = loc - 1
            for i in reversed(range(loc - 1)):
                if temp[i] not in [',', ' ', '(']:
                    f_start = i
                else:
                    break
            func_name = temp[f_start:loc]

            # Replace the original function call and add back whatever
            # was on the outside of the function call.
            expr = temp[loc+1:bo]
            temp = temp[:f_start] + ''.join([func_name + '('] * eval(expr)) + \
            temp[bo+1:bc] + ''.join([')'] * eval(expr)) + temp[bc+1:]

    # Return the expanded version of the rule.
    return temp

# Take file name from the command line and read all lines from the file.
inp = sys.argv[1] if len(sys.argv) > 1 else 'please_specify_input_path.trs'
outp = sys.argv[2] if len(sys.argv) > 2 else inp[:inp.rfind('.')] + '.trs'
lines = [line.rstrip('\n').lstrip() for line in open(inp)]
temp_lines = []

# Iterate over the lines and expand them if needed.
for line in lines:
    x1 = line.find('[')
    if x1 == -1:
        temp_lines += [line]
    else:
        # Extract variables and ranges.
        y1 = line.find(']')
        x2 = line.find('{')
        y2 = line.find('}')
        rule = line[y2+1:].lstrip()
        variables = [v.strip() for v in line[x1+1:y1].split(',')]
        values = [int(value) for value in line[x2+1:y2].split(',')]
        var_range = range(values[0], values[1] + 1)

        # Loop over all tuples and instantiate occurances of the variables.
        for element in product(*[var_range] * len(variables)):
            temp_line = rule
            for i, var in enumerate(variables):
                temp_line = temp_line.replace(var, str(element[i]))
            temp_lines += [temp_line]

# Apply the meaning of *, ', and ` in the rules.
temp_lines2 = []
for line in temp_lines:
    chars = list(line.replace('=', '->'))
    while(True):
        try:
            loc = chars.index('*')
            chars[loc-1] = str(10 - int(chars[loc-1]))
            chars.remove('*')
        except ValueError:
            break
    while(True):
        try:
            loc = chars.index('`')
            chars[loc-1] = str(int(chars[loc-1]) - 1)
            chars.remove('`')
        except ValueError:
            break
    while(True):
        try:
            loc = chars.index("'")
            chars[loc-1] = str(int(chars[loc-1]) + 1)
            chars.remove("'")
        except ValueError:
            break
    temp_lines2 += [''.join(chars)]

# Expand function calls and write the new rules to the new file.
new_file = open(outp, 'w')
for i, new_line in enumerate([exp_funcs(tl) for tl in temp_lines2]):
    opened = 0
    # Superficially check whether opening and closing brackets match.
    if new_line == '(RULES' or new_line == ')':
        pass
    else:
        for c in new_line:
            if c == '(':
                opened += 1
            elif c == ')':
                opened -= 1
                if opened < 0:
                    print('Unopened bracket(s) on line ' + str(i+1))
                    break
        if opened > 0:
            print('Unclosed bracket(s) on line ' + str(i+1))
    new_file.write(new_line+'\n')

print('Done writing to ' + outp + '.')
