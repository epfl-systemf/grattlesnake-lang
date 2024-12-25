
def f(x):
    if x < 0:
        if x >= -5:
            return -12
        elif x > -11:
            if x != -7:
                return -742
            else:
                return 1521
        else:
            return 207
    elif x == 0:
        return -75
    else:
        if (x > 25 and x <= 35) or x == 100:
            return 100
        else:
            return 1005

if __name__ == "__main__":
    with open("./stdout.exp", "w") as stdout_file:
        for i in range(-100, 120):
            print(f(i), file=stdout_file, end="\n")
