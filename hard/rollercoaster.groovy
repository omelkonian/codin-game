try {
    input = new Scanner(new File("/home/orestis/git/roller-coaster/input.txt"))
} catch (FileNotFoundException e) {
    input = new Scanner(System.in)
}

gr = []

places = input.nextInt()
iters = input.nextInt()
groupNum = input.nextInt()

start = 0
end = 0
currentPeople = 0

Tuple[] next = new Tuple[groupNum]

groupNum.times {
    gr += input.nextInt()
    if (end == it && gr[it] + currentPeople <= places) {
        currentPeople += gr[it]
        end++;
    }
}

totalPeople = gr.sum()

if (totalPeople <= places) {
    println totalPeople * iters
    return
}

next[0] = new Tuple(end, currentPeople)

(1..<groupNum).each {
    currentPeople -= gr[it - 1]
    start++;
    while (currentPeople + gr[end] <= places) {
        currentPeople += gr[end]
        end++
        end %= groupNum
    }
    next[it] = new Tuple(end, currentPeople)
}

Tuple[] curS = new Tuple[groupNum]
long cur = 0
long res = 0
for (long i = 0; i < iters; i++) {
    if (curS[cur] == null) {
        curS[cur] = new Tuple(i, res)
        res += next[cur][1]
        cur = next[cur][0]
    } else {
        long lastRound = curS[cur][0]
        long lastProfit = curS[cur][1]
        long loopPrice = res - lastProfit
        long loopRounds = i - lastRound
        long remainingRounds = iters - i
        long loopTimes = remainingRounds.intdiv(loopRounds)
        if (loopTimes == 0) {
            res += next[cur][1]
            cur = next[cur][0]
        } else {
            res += loopTimes * loopPrice
            i += loopTimes * loopRounds - 1
        }
    }
}

println res