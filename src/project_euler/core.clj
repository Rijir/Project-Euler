(ns project-euler.core
  (:use clojure.math clojure.set)
  (:require clojure.string
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combinatorics]))

(defn problem0
  []
  (reduce +
          (map #(* % %)
               (map bigint (range 1 980000 2)))))

(defn problem1
  []
  (reduce +
          (filter #(or (= (mod % 3) 0)
                       (= (mod % 5) 0))
                  (range 1000))))

(defn fib
  ([]
   (fib 1N 1N))
  ([a b]
   (lazy-seq (cons a (fib b (+ a b))))))

(defn problem2
  []
  (let [upper-bound 4000000]
    (reduce +
            (filter even?
                    (take-while #(< % upper-bound) (fib))))))

(defn primes-under
  "simple sieve of eratosthenes,
  not fully efficient because it checks every remaining candidate mod the prime,
  instead of only eliminating multiples of the prime"
  ([n]
   (primes-under n (range 2 n)))
  ([n c]
   (if (not (empty? c))
     (let [p (first c)]
       (lazy-seq
        (cons p
              (primes-under n
                            (doall (filter #(not (= (mod % p) 0))
                                           (rest c))))))))))

(defn sieve-of-eratosthenes
  "Sieve of eratosthenes, see http://www.learningclojure.com/2009/11/sieve-of-eratosthenes.html"
  ([n]
   (sieve-of-eratosthenes (sorted-set) (apply sorted-set (range 2 (inc n))) (inc n)))
  ([ps c end]
   (if-let [p (first c)]
     (recur (conj ps p)
            (difference c (range p end p))
            end)
     ps)))

(defn problem3
  "The prime factors of 13195 are 5, 7, 13, and 29.
   What is the largest prime factor of the number 600851475143?"
  ([]
   (problem3 600851475143))
  ([n]
   (let [p (sieve-of-eratosthenes (sqrt n))
         f (filter #(= (mod n %) 0) p)]
     (last f))))

(defn palindrome-number?
  [n]
  (let [dig-count (inc (floor (log10 n)))]
    (every?  #(= (mod (floor (/ n (pow 10 %))) 10)
                 (mod (floor (/ n (pow 10 (- dig-count % 1)))) 10))
             (range (/ dig-count 2)))))

(defn problem4
  "A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 x 99.
   Find the largest palindrome made from the product of two 3-digit numbers."
  []
  ;; (loop [a 999
  ;;        b 999]
  ;;   (let [prod (* a b)]
  ;;     (if (palindrome-number? prod)
  ;;       (str a "x" b "=" prod)
  ;;       (if (> b 99)
  ;;         (recur a (dec b))
  ;;         (recur (dec a) 999)))))
  (let [factors (range 100 1000)
        products (reduce concat
                         (map (fn [a]
                                (map (fn [b] (* a b))
                                     factors))
                              factors))
        palindromes (filter palindrome-number? products)]
    (apply max palindromes))
  )

(defn problem5
  "2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
   What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?"
  []
  (let [upper-bound 20
        primes (sieve-of-eratosthenes upper-bound)
        prime-powers (map #(loop [e 1]
                             (if (> (pow % (inc e)) upper-bound)
                               (bigint (pow % e))
                               (recur (inc e))))
                          primes)]
    (reduce * prime-powers)))

(defn problem6
  "The sum of the squares of the first ten natural numbers is,
    1^2 + 2^2 + ... + 10^2 = 385.
   The square of the sum of the first ten natural numbers is,
    (1 + 2 + ... + 10)^2 = 55^2 = 3025.
   Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is
    3025 - 385 = 2640.
   Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum."
  []
  (let [upper-bound (inc 100)]
    (- (bigint (pow (reduce + (range upper-bound)) 2))
       (reduce + (map #(* % %) (range upper-bound))))))

(defn problem7
  "By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
   What is the 10,001st prime number?"
  []
  (nth (vec (apply sorted-set (sieve-of-eratosthenes 1000000)))
       (dec 10001)
       "Increase upper bound!"))

(defn problem8
  "The four adjacent digits in the 1000-digit number that have the greatest product are 9 x 9 x 8 x 9 = 5832 .
    73167176531330624919225119674426574742355349194934
    96983520312774506326239578318016984801869478851843
    85861560789112949495459501737958331952853208805511
    12540698747158523863050715693290963295227443043557
    66896648950445244523161731856403098711121722383113
    62229893423380308135336276614282806444486645238749
    30358907296290491560440772390713810515859307960866
    70172427121883998797908792274921901699720888093776
    65727333001053367881220235421809751254540594752243
    52584907711670556013604839586446706324415722155397
    53697817977846174064955149290862569321978468622482
    83972241375657056057490261407972968652414535100474
    82166370484403199890008895243450658541227588666881
    16427171479924442928230863465674813919123162824586
    17866458359124566529476545682848912883142607690042
    24219022671055626321111109370544217506941658960408
    07198403850962455444362981230987879927244284909188
    84580156166097919133875499200524063689912560717606
    05886116467109405077541002256983155200055935729725
    71636269561882670428252483600823257530420752963450
   Find the thirteen adjacent digits in the 1000-digit number that have the greatest product. What is the value of this product?"
  []
  (let [digits-str "73167176531330624919225119674426574742355349194934
    96983520312774506326239578318016984801869478851843
    85861560789112949495459501737958331952853208805511
    12540698747158523863050715693290963295227443043557
    66896648950445244523161731856403098711121722383113
    62229893423380308135336276614282806444486645238749
    30358907296290491560440772390713810515859307960866
    70172427121883998797908792274921901699720888093776
    65727333001053367881220235421809751254540594752243
    52584907711670556013604839586446706324415722155397
    53697817977846174064955149290862569321978468622482
    83972241375657056057490261407972968652414535100474
    82166370484403199890008895243450658541227588666881
    16427171479924442928230863465674813919123162824586
    17866458359124566529476545682848912883142607690042
    24219022671055626321111109370544217506941658960408
    07198403850962455444362981230987879927244284909188
    84580156166097919133875499200524063689912560717606
    05886116467109405077541002256983155200055935729725
    71636269561882670428252483600823257530420752963450"
        digits (->> digits-str
                    (filter #(and (not= % \newline)
                                  (not= % \space)))
                    (map str)
                    (map Integer/parseInt))
        adj-count 13
        digit-count (count digits)]
    (loop [max-prod 0
           start 0]
      (if (> (+ start adj-count) digit-count)
        max-prod
        (let [prod (reduce * (map #(nth digits (+ start %))
                                  (range adj-count)))]
          (if (> prod max-prod)
            (recur prod (inc start))
            (recur max-prod (inc start))))))))

(defn pythagorean-triplet
  "Given a and b, returns c iff a^2 + b^2 = c^2 and a, b, and c are all natural numbers.
   Else returns nil"
  [a b]
  (let [c (sqrt (+ (* a a) (* b b)))]
    (if (= (mod c 1) 0.0)
      (int c))))

(defn problem9
  "A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
    a^2 + b^2 = c^2.
   For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product abc."
  []
  (let [target-sum 1000
        triplets (->> (range 1 (/ target-sum 2))
                      (map (fn [a] (map (fn [b] [a b]) (range a (/ target-sum 2)))))
                      (reduce concat)
                      (map #(let [c (apply pythagorean-triplet %)]
                              (if c (concat % [c])
                                  nil)))
                      (filter #(not= % nil))
                      (filter #(= target-sum (reduce + %))))]
    (reduce * (first triplets))))

(defn problem10
  "The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
   Find the sum of all the primes below two million."
  []
  (let [upper-bound 2000000]
    (reduce + (sieve-of-eratosthenes upper-bound))))

(defn problem11
  "In the 20x20 grid below, four numbers along a diagonal line have been marked in red.
    08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
    49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
    81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
    52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
    22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
    24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
    32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
    67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
    24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
    21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
    78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
    16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
    86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
    19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
    04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
    88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
    04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
    20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
    20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
    01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48
   The product of these numbers is 26 \times 63 \times 78 \times 14 = 1788696.
   What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20x20 grid?"
  []
  (let [grid-str
        "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
         49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
         81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
         52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
         22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
         24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
         32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
         67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
         24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
         21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
         78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
         16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
         86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
         19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
         04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
         88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
         04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
         20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
         20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
         01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"
        grid-rows (clojure.string/split-lines grid-str)
        grid (mapv #(mapv (fn [n] (Integer/parseInt n))
                          (clojure.string/split (clojure.string/trim %) #" "))
                   grid-rows)
        hprods (reduce concat
                       (map (fn [row]
                              (map (fn [start-col]
                                     (reduce *
                                             (map (fn [col]
                                                    (get-in grid [row col]))
                                                  (range start-col (+ start-col 4)))))
                                   (range (- (count grid) (dec 4)))))
                            (range (count grid))))
        vprods (reduce concat
                       (map (fn [col]
                              (map (fn [start-row]
                                     (reduce *
                                             (map (fn [row]
                                                    (get-in grid [row col]))
                                                  (range start-row (+ start-row 4)))))
                                   (range (- (count grid) (dec 4)))))
                            (range (count grid))))
        ;; forward diagonal like /
        fprods (reduce concat
                       (map (fn [start-row]
                              (map (fn [start-col]
                                     (reduce *
                                             (map (fn [i]
                                                    ;; (println "Getting " [(- start-row i) (+ start-col i)])
                                                    (get-in grid [(- start-row i) (+ start-col i)]))
                                                  (range 4))))
                                   (range 17)))
                            (range 3 20)))
        ;; back diagonal like \
        bprods (reduce concat
                       (map (fn [start-row]
                              (map (fn [start-col]
                                     (reduce *
                                             (map (fn [i]
                                                    (get-in grid [(+ start-row i) (+ start-col i)]))
                                                  (range 4))))
                                   (range 17)))
                            (range 16)))]
    (apply max (concat hprods vprods fprods bprods))))

(defn triangle-numbers
  ([] (triangle-numbers 0 1))
  ([c ;; the previous triangle number
    n ;; the current natural number
    ]
   (let [t (+ c n) ;; the next triangle number
         ]
     (lazy-seq (cons t
                     (triangle-numbers t (inc n)))))))

(defn triangle-number?
  "See https://stackoverflow.com/questions/2913215/fastest-method-to-define-whether-a-number-is-a-triangular-number"
  [n]
  (= (mod (sqrt (+ 1 (* 8 n))) 1)
     0.0))

(defn divisors
  [n]
  (let [limit (sqrt n)]
    (loop [c (range 1 (inc (int limit)))
           ds []]
      (if-let [i (first c)]
        (if (= (mod n i) 0)
          (if (= i (/ n i))
            (recur (rest c) (conj ds i))
            (recur (rest c) (conj ds i (/ n i))))
          (recur (rest c) ds))
        ds))))

(defn problem12
  "The sequence of triangle numbers is generated by adding the natural numbers. So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:
    1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
   Let us list the factors of the first seven triangle numbers:
    \\begin{align}
        \\mathbf 1   &\\colon 1       \\
        \\mathbf 3   &\\colon 1,3     \\
        \\mathbf 6   &\\colon 1,2,3,6 \\
        \\mathbf{10} &\\colon 1,2,5,10\\
        \\mathbf{15} &\\colon 1,3,5,15\\
        \\mathbf{21} &\\colon 1,3,7,21\\
        \\mathbf{28} &\\colon 1,2,4,7,14,28
    \\end{align}

   We can see that 28 is the first triangle number to have over five divisors.
   What is the value of the first triangle number to have over five hundred divisors?"
  []
  (let [;; The number of factors of a number is equal to the sum of the (inc power) in the prime factorization
        ;; 500 = 2^2 x 5^3
        ;;     = (1+1)^2 x (4+1)^3
        ;; So the prime factorization of the lowest number with 500 factors has 2 1st powers and 3 4th powers
        ;; 2^4 x 3^4 x 5^4 x 7 x 11 = 62370000 minimizes
        start (* (pow 2 4) (pow 3 4) (pow 5 4) 7 11)]
    (->> (triangle-numbers)
         (drop-while #(< % start))
         (filter #(> (count (divisors %)) 500))
         (first))))

(defn problem13
  "Work out the first ten digits of the sum of the following one-hundred 50-digit numbers."
  []
  (let [input "37107287533902102798797998220837590246510135740250
46376937677490009712648124896970078050417018260538
74324986199524741059474233309513058123726617309629
91942213363574161572522430563301811072406154908250
23067588207539346171171980310421047513778063246676
89261670696623633820136378418383684178734361726757
28112879812849979408065481931592621691275889832738
44274228917432520321923589422876796487670272189318
47451445736001306439091167216856844588711603153276
70386486105843025439939619828917593665686757934951
62176457141856560629502157223196586755079324193331
64906352462741904929101432445813822663347944758178
92575867718337217661963751590579239728245598838407
58203565325359399008402633568948830189458628227828
80181199384826282014278194139940567587151170094390
35398664372827112653829987240784473053190104293586
86515506006295864861532075273371959191420517255829
71693888707715466499115593487603532921714970056938
54370070576826684624621495650076471787294438377604
53282654108756828443191190634694037855217779295145
36123272525000296071075082563815656710885258350721
45876576172410976447339110607218265236877223636045
17423706905851860660448207621209813287860733969412
81142660418086830619328460811191061556940512689692
51934325451728388641918047049293215058642563049483
62467221648435076201727918039944693004732956340691
15732444386908125794514089057706229429197107928209
55037687525678773091862540744969844508330393682126
18336384825330154686196124348767681297534375946515
80386287592878490201521685554828717201219257766954
78182833757993103614740356856449095527097864797581
16726320100436897842553539920931837441497806860984
48403098129077791799088218795327364475675590848030
87086987551392711854517078544161852424320693150332
59959406895756536782107074926966537676326235447210
69793950679652694742597709739166693763042633987085
41052684708299085211399427365734116182760315001271
65378607361501080857009149939512557028198746004375
35829035317434717326932123578154982629742552737307
94953759765105305946966067683156574377167401875275
88902802571733229619176668713819931811048770190271
25267680276078003013678680992525463401061632866526
36270218540497705585629946580636237993140746255962
24074486908231174977792365466257246923322810917141
91430288197103288597806669760892938638285025333403
34413065578016127815921815005561868836468420090470
23053081172816430487623791969842487255036638784583
11487696932154902810424020138335124462181441773470
63783299490636259666498587618221225225512486764533
67720186971698544312419572409913959008952310058822
95548255300263520781532296796249481641953868218774
76085327132285723110424803456124867697064507995236
37774242535411291684276865538926205024910326572967
23701913275725675285653248258265463092207058596522
29798860272258331913126375147341994889534765745501
18495701454879288984856827726077713721403798879715
38298203783031473527721580348144513491373226651381
34829543829199918180278916522431027392251122869539
40957953066405232632538044100059654939159879593635
29746152185502371307642255121183693803580388584903
41698116222072977186158236678424689157993532961922
62467957194401269043877107275048102390895523597457
23189706772547915061505504953922979530901129967519
86188088225875314529584099251203829009407770775672
11306739708304724483816533873502340845647058077308
82959174767140363198008187129011875491310547126581
97623331044818386269515456334926366572897563400500
42846280183517070527831839425882145521227251250327
55121603546981200581762165212827652751691296897789
32238195734329339946437501907836945765883352399886
75506164965184775180738168837861091527357929701337
62177842752192623401942399639168044983993173312731
32924185707147349566916674687634660915035914677504
99518671430235219628894890102423325116913619626622
73267460800591547471830798392868535206946944540724
76841822524674417161514036427982273348055556214818
97142617910342598647204516893989422179826088076852
87783646182799346313767754307809363333018982642090
10848802521674670883215120185883543223812876952786
71329612474782464538636993009049310363619763878039
62184073572399794223406235393808339651327408011116
66627891981488087797941876876144230030984490851411
60661826293682836764744779239180335110989069790714
85786944089552990653640447425576083659976645795096
66024396409905389607120198219976047599490197230297
64913982680032973156037120041377903785566085089252
16730939319872750275468906903707539413042652315011
94809377245048795150954100921645863754710598436791
78639167021187492431995700641917969777599028300699
15368713711936614952811305876380278410754449733078
40789923115535562561142322423255033685442488917353
44889911501440648020369068063960672322193204149535
41503128880339536053299340368006977710650566631954
81234880673210146739058568557934581403627822703280
82616570773948327592232845941706525094512325230608
22918802058777319719839450180888072429661980811197
77158542502016545090413245809786882778948721859617
72107838435069186155435662884062257473692284509516
20849603980134001723930671666823555245252804609722
53503534226472524250874054075591789781264330331690"
        ns (->> (clojure.string/split-lines input)
                (map bigint))
        sum (reduce + ns)]
    (apply str (take 10 (str sum)))))

(def collatz
  (memoize
   (fn collatz-impl [n]
     (if (= n 1)
       (list n)
       (lazy-seq (cons n (collatz
                          (if (= (mod n 2) 0)
                            (/ n 2)
                            (+ (* 3 n) 1)))))))))

(defn problem14
  "The following iterative sequence is defined for the set of positive integers:
    n -> n/2 (n is even)
    n -> 3n + 1(is odd)
   Using the rule above and starting with 13, we generate the following sequence:
   It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

   Which starting number, under one million, produces the longest chain?

   NOTE: Once the chain starts the terms are allowed to go above one million."
  []
  (loop [i 2
         m [1 1 '(1)]]
    (if (>= i 1000000)
      (print "Answer: " (second m) "\n  Collatz sequence is " (first m) " terms long:\n" (last m))
      (let [c (collatz i)
            l (count c)
            n (inc i)]
        (if (> l (first m))
          (recur n [l i c])
          (recur n m))))))

(defn factorial
  ([n] (factorial (bigint n) 1N))
  ([n c]
   (if (> n 1)
     (recur (dec n) (* c n))
     c)))

(defn choose
  "Number of ways to choose an (unordered) subset of k elements from a fixed set of n elements"
  [n k]
  (/ (factorial n)
     (* (factorial k)
        (factorial (- n k)))))

(defn problem15
  "Starting in the top left corner of a 2x2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
   --
     |
     |
     v
  -
   |_
     |
     v

  -
   |
   |_>

  |
  --
    |
    v

  |
  -
   |_>

  |
  |
   -->

   How many such routes are there through a 20x20 grid?"
  []
  ;; In a NxN grid, there are 2N lines: N horizontal and N vertical.
  ;; We must choose where to place N vertical (or equivalently horizontal) lines within the 2N long sequence.
  ;; So the answer is 40 choose 20
  (choose 40 20))

(defn problem16
  "2^{15} = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

   What is the sum of the digits of the number 2^{1000}?"
  []
  (let [n (reduce * (repeat 1000 2N))
        s (str n)
        digits (map #(Integer/parseInt (str %)) s)]
    (println "n=" n)
    (reduce + digits)))

(def ones-words ["" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])
(def teens-words ["ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"])
(def tens-words ["" 'special "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"])
(def hundreds-words (apply conj [""] (map #(str % "hundred") (drop 1 ones-words))))
(def thousands-words ["" "onethousand"])

(defn number-to-english [n]
  (if (= n 0)
    "zero"
    (let [thousands (mod (int (/ n 1000)) 10)
          hundreds (mod (int (/ n 100)) 10)
          tens (mod (int (/ n 10)) 10)
          ones (mod n 10)]
      ;; (pprint [thousands hundreds tens ones])
      (str (nth thousands-words thousands)
           (nth hundreds-words hundreds)
           (if (and (or (> thousands 0) (> hundreds 0))
                    (not= 0 tens ones))
             (str "and"))
           (if (= tens 1)
             (nth teens-words ones)
             (str (nth tens-words tens)
                  (nth ones-words ones)))))))

(defn problem17
  "If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

    If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

    NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters.
    The use of \"and\" when writing out numbers is in compliance with British usage."
  []
  (loop [c 0
         i 1]
    (if (> i 1000)
      c
      (recur (+ c (count (number-to-english i)))
             (inc i)))))

(defn max-triangle-path
  [input]
  (let [rows (map #(mapv Integer/parseInt (clojure.string/split % #" ")) (clojure.string/split-lines input))
        rows-rev (reverse rows)]
    (loop [m (first rows-rev) ;; current maximum sum from the bottom up
           r (rest rows-rev) ;; remaining rows
           ]
      (if (empty? r)
        (apply max m)
        (let [c (first r) ;; current row
              ]
          (recur (map #(+ (nth c %) ;; add the maximum adjacent sum from m to the number in c
                          (max (nth m %)
                               (nth m (inc %))))
                      (range (count c)))
                 (rest r)))))))

(defn problem18
  "By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

    3
    7 4
    2 4 6
    8 5 9 3

    That is, 3 + 7 + 4 + 9 = 23.

    Find the maximum total from top to bottom of the triangle below:

    75
    95 64
    17 47 82
    18 35 87 10
    20 04 82 47 65
    19 01 23 75 03 34
    88 02 77 73 07 63 67
    99 65 04 28 06 16 70 92
    41 41 26 56 83 40 80 70 33
    41 48 72 33 47 32 37 16 94 29
    53 71 44 65 25 43 91 52 97 51 14
    70 11 33 28 77 73 17 78 39 68 17 57
    91 71 52 38 17 14 91 43 58 50 27 29 48
    63 66 04 68 89 53 67 30 73 16 69 87 40 31
    04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

    NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route.
    However, Problem 67, is the same challenge with a triangle containing one-hundred rows;
    it cannot be solved by brute force, and requires a clever method! ;o)"
  []
  (let [input "75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
"]
    (max-triangle-path input)))

(defn problem19
  "You are given the following information, but you may prefer to do some research for yourself.

    1 Jan 1900 was a Monday.

    Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.

    A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

    How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?"
  []
  (let [month-lengths     [31 28 31 30 31 30 31 31 30 31 30 31]
        year-length (reduce + month-lengths)
        leap-month-lenths [31 29 31 30 31 30 31 31 30 31 30 31]
        leap-year-length (reduce + leap-month-lenths)
        leap-year? (fn [y]
                     (and (= 0 (mod y 4)) ;; every fourth year
                          (not (and (= 0 (mod y 100)) ;; except centenials
                                    (not (= 0 (mod y 400))) ;; unless it's divisible by 400
                                    ))))
        ]
    (println "day/month/year")
    (loop [latest-year-start 1 ;; in days
           latest-year 1900
           latest-month-start 1 ;; in days
           latest-month 0 ;; index in month-lengths or leap-month-lengths (add one for calendar date)
           latest-sunday 7
           sunday-the-first-count 0
           ]
      (if (> latest-year 2000)
        sunday-the-first-count
        (let [next-sunday (+ latest-sunday 7)
              leap? (leap-year? latest-year)
              next-month-start (+ latest-month-start
                                  (nth (if leap? leap-month-lenths month-lengths)
                                       latest-month))
              next-month (inc latest-month)
              next-year-start (+ latest-year-start
                                 (if leap? leap-year-length year-length))
              ]
          (when (and (> latest-year 1900)(= next-sunday next-month-start))
            ;; debug info
            (println "1/"
                     (inc (mod next-month (count month-lengths))) "/"
                     (if (>= next-month (count month-lengths))
                       (inc latest-year)
                       latest-year)
                     " was a Sunday"))
            (do
            (if (>= next-sunday next-month-start)
              ;; the next sunday is in the next month, but not the 1st
              (recur (if (>= next-month (count month-lengths))
                       next-year-start
                       latest-year-start)
                     (if (>= next-month (count month-lengths))
                       (inc latest-year)
                       latest-year)
                     next-month-start
                     (mod next-month (count month-lengths))
                     next-sunday
                     (if (and (> latest-year 1900)(= next-sunday next-month-start))
                       ;; the next sunday is the 1st of the month
                       (inc sunday-the-first-count)
                       sunday-the-first-count))
              ;; the next sunday is still in the current month
              (recur latest-year-start
                     latest-year
                     latest-month-start
                     latest-month
                     next-sunday
                     sunday-the-first-count))))))))

(defn problem20
  "n! means n x (n - 1) x ... x 3 x 2 x 1.

    For example, 10! = 10 x 9 x ... x 3 x 2 x 1 = 3628800,
    and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

    Find the sum of the digits in the number 100!."
  []
  (let [n (factorial 100)
        digits (map #(Integer/parseInt (str %)) (str n))]
    (reduce + digits)))

(defn divisors-sum
  [n]
  (let [divs (divisors n)
                  proper-divs (filter #(not= % n) divs)]
              (reduce + proper-divs)))

(defn problem21
  "Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
    If d(a) = b and d(b) = a, where a!=b, then a and b are an amicable pair and each of a and b are called amicable numbers.

    For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71, and 142; so d(284) = 220.

    Evaluate the sum of all the amicable numbers under 10000."
  []
  (letfn [(d [n]
            (divisors-sum n))
          (amicable? [a]
            (let [b (d a)
                  db (d b)]
              (and (not= a b)
                   (= db a))))]
    (let [amicable (filter amicable? (range 10000))]
      (reduce + amicable))))

(defn word-value [word]
  (reduce +
          (map (fn [c]
                 (- (int c)
                    (dec (int \A))))
               word)))

(defn problem22
  "Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order.
    Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

    For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of
    938 x 53 = 49714.

    What is the total of all the name scores in the file?"
  []
  (let [input (slurp (io/resource "0022_names.txt"))
        names (sort (clojure.string/split (clojure.string/replace input #"\"" "") #","))]
    (->> names
         (map
          (fn [i name]
            (* i
               (word-value name)))
          (iterate inc 1))
         (reduce +))))

(defn abundant?-impl
  "True iff (divisors-sum n) > n"
  [n]
  (> (divisors-sum n) n))

(def abundant?
  "True iff (divisors-sum n) > n"
  (memoize abundant?-impl))

(defn abundant-sum?
  [n]
  (reduce (fn [a b] (or a b))
          false
          (map (fn [a]
                 (and (abundant? a)
                      (abundant? (- n a))))
               (range 12 (inc (/ n 2))))))

(defn problem23
  "A perfect number is a number for which the sum of its proper divisors is exactly equal to the number.
    For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

    A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

    As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24.
    By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers.
    However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

    Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers."
  []
  (let [candidates (range 1 28123)
        abundants (filter abundant? candidates)
        abundant-sums (filter abundant-sum? candidates)
        non-abundant (filter #(not (abundant-sum? %)) candidates)]
    (reduce + non-abundant)))

(defn lexicographic-permutations
  "col must be sorted"
  [col]
  (if (empty? col)
    [[]]
    (lazy-seq
     (apply concat
            (map (fn [x]
                   (map #(cons x %)
                        (lexicographic-permutations
                         (filter #(not= % x) col))))
                 col)))))

(defn problem24
  "A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4.
    If all of the permutations are listed numerically or alphabetically, we call it lexicographic order.
    The lexicographic permutations of 0, 1 and 2 are:

    012   021   102   120   201   210

    What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?"
  []
  (apply str (nth (lexicographic-permutations [0 1 2 3 4 5 6 7 8 9]) (dec 1000000))))

(defn num-digits
  [n]
  (if (int? n)
    (inc (int (log10 n)))
    (count (str n))))

(defn problem25
  "The Fibonacci sequence is defined by the recurrence relation:
    F_n = F_{n - 1} + F_{n - 2}, where F_1 = 1 and F_2 = 1.

    Hence the first 12 terms will be:
    begin{align}
    F_1 &= 1
    F_2 &= 1
    F_3 &= 2
    F_4 &= 3
    F_5 &= 5
    F_6 &= 8
    F_7 &= 13
    F_8 &= 21
    F_9 &= 34
    F_{10} &= 55
    F_{11} &= 89
    F_{12} &= 144
    end{align}
    The 12th term, F_{12}, is the first term to contain three digits.

    What is the index of the first term in the Fibonacci sequence to contain 1000 digits?"
  []
  (let [fib-indexed (map (fn [i n] [i n]) (iterate inc 1) (fib))]
    (first (first (filter (fn [[i n]] (>= (num-digits n) 1000))
                          fib-indexed)))))

(defn remainder-seq
  [n d]
  (loop [n n
         rs []
         seen {}
         i 0]
    (let [rem (mod n d)
          seen-i (seen rem)]
      (if (not (nil? seen-i))
        {:seq rs
         :cycle {:from seen-i
                 :to i
                 :length (- i seen-i)}}
        (recur (* rem 10)
               (conj rs rem)
               (assoc seen rem i)
               (inc i))))))

(defn problem26
  "A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:
    begin{align}
    1/2 &= 0.5
    1/3 &=0.(3)
    1/4 &=0.25
    1/5 &= 0.2
    1/6 &= 0.1(6)
    1/7 &= 0.(142857)
    1/8 &= 0.125
    1/9 &= 0.(1)
    1/10 &= 0.1
    end{align}
    Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.
    Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part."
  []
  (loop [d 2
         max-d nil
         max-len 0]
    (if (>= d 1000)
      {:answer max-d :length max-len}
      (let [{{length :length} :cycle} (remainder-seq 1 d)]
        (recur (inc d)
               (if (> length max-len)
                 d
                 max-d)
               (if (> length max-len)
                 length
                 max-len))))))


(def prime-cache (atom {:limit 10000
                        :primes (sieve-of-eratosthenes 10000)}))
(defn prime?
  [n]
  (if (< n 0)
    false
    (if (< n (:limit @prime-cache))
      ((:primes @prime-cache) n)
      (let [limit (inc (int (sqrt n)))]
        (when (< (:limit @prime-cache) limit)
          (reset! prime-cache {:limit limit :primes (sieve-of-eratosthenes limit)}))
        (loop [c (:primes @prime-cache)]
          (if-let [i (first c)]
            (if (>= i n)
              true ;; we passed p without finding a divisor
              (if (= (mod n i) 0)
                false ;; found a divisor
                (recur (rest c))))
            true ;; exhausted possible prime factors
            ))))))

;; (defn prime? [n]
;;   (= 2 (count (divisors n))))

(defn consecutive-primes-length
  [a b]
  (loop [n 0]
    (if (prime? (+ (* n n) (* a n) b))
      (recur (inc n))
      n)))

(defn problem27
  "
    Euler discovered the remarkable quadratic formula:
        n^2 + n + 41
    It turns out that the formula will produce 40 primes for the consecutive integer values 0 <= n <= 39.
    However, when n=40, 40^2+40+41 = 40(40+1)+41 is divisible by 41, and certainly when n=41, 41^2 + 41 + 41 is clearly divisible by 41.

    The incredible formula n^2 - 79n + 1601 was discovered, which produces 80 primes for the consecutive values 0 <= n <= 79. The product of the coefficients, -79 and 1601, is -126479.

    Considering quadratics of the form:

        n^2 + an + b, where |a| < 1000 and |b| <= 1000

    where |n| is the modulus/absolute value of n
    e.g. |11| = 11 and |-4| = 4

    Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n=0.
  "
  []
  (let [primes (set (sieve-of-eratosthenes 1000))]
    (loop [a -999
           max {:a 1 :b 41 :len 40}]
      (if (> a 999)
        (assoc max :product (* (:a max) (:b max)))
        (let [{b :b len :len}
              (loop [b (first primes)
                     primes (rest primes)
                     max {:b nil :len 0}]
                (let [l (consecutive-primes-length a b)
                      new-max (if (> l (:len max)) {:b b :len l} max)]
                  (if (empty? primes)
                    new-max
                    (recur (first primes)
                           (rest primes)
                           new-max))))
              new-max (if (> len (:len max)) {:a a :b b :len len} max)]
          (recur (inc a)
                 new-max))))))

(defn spiral-diagonals
  ;; n must be odd
  [n]
  (if (= n 1)
    [1]
    (let [nsquared (* n n)
          dif (dec n)]
      (take 4 (iterate #(- % dif) nsquared)))))

(defn problem28
  "
    Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:

        21 22 23 24 25
        20  7  8  9 10
        19  6  1  2 11
        18  5  4  3 12
        17 16 15 14 13

    It can be verified that the sum of the numbers on the diagonals is 101.

    What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?"
  []
  (reduce + (flatten (map spiral-diagonals (range 1 1002 2)))))

(defn problem29
  "
    Consider all integer combinations of a^b for 2<=a<=5 and 2<=b<=5:
        begin{array}{rrrr}
        2^2=4, &2^3=8, &2^4=16, &2^5=32
        3^2=9, &3^3=27, &3^4=81, &3^5=243
        4^2=16, &4^3=64, &4^4=256, &4^5=1024
        5^2=25, &5^3=125, &5^4=625, &5^5=3125
        end{array}
    If they are then placed in numerical order, with any repeats removed, we get the following sequence of 15 distinct terms:
    How many distinct terms are in the sequence generated by for and ?"
  []
  (count (reduce (fn [terms a]
                   (reduce (fn [terms b]
                             (conj terms (pow a b)))
                           terms
                           (range 2 101)))
                 #{}
                 (range 2 101))))

(defn digits
  [n]
  (map #(Integer/parseInt (str %)) (str n)))

(defn problem30
  "
    Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
        begin{align}
        1634 &= 1^4 + 6^4 + 3^4 + 4^4
        8208 &= 8^4 + 2^4 + 0^4 + 8^4
        9474 &= 9^4 + 4^4 + 7^4 + 4^4
        end{align}
    As 1=1^4 is not a sum it is not included.

    The sum of these numbers is 1634 + 8208 + 9474 = 19316.

    Find the sum of all the numbers that can be written as the sum of fifth powers of their digits."
  ([] (problem30 5)) ;; Allow for testing with p=4
  ([p]
   (let [digit-power-sum?
         (fn [n]
           (let [ds (digits n)]
             (= n
                (reduce + (map #(int (pow % p)) ds)))))]
     (reduce + (filter digit-power-sum? (range 2 (pow 10 (inc p))))))))

(defn problem31
  "
    In the United Kingdom the currency is made up of pound (£) and pence (p). There are eight coins in general circulation:
        1p, 2p, 5p, 10p, 20p, 50p, £1 (100p), and £2 (200p).
    It is possible to make £2 in the following way:
        1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
    How many different ways can £2 be made using any number of coins?"
  []
  (let [coins [1 2 5 10 20 50 100 200]]
    (letfn [(coin-perms
              [i remaining amounts]
              (if (>= i (count coins))
                (if (= remaining 0)
                  [amounts]
                  [])
                (let [coin-value (nth coins i)
                      max-coins-of-value (int (/ remaining coin-value))]
                  (reduce concat
                          (map #(coin-perms (inc i)
                                            (- remaining (* coin-value %))
                                            (assoc amounts i %))
                               (range (inc max-coins-of-value)))))))]
      (count (coin-perms 0
                         200
                         (vec (repeat (count coins) 0)))))))

(defn divisor-pairs
  [n]
  (loop [remaining (divisors n)
         ds []]
    (if (empty? remaining)
      ds
      (recur (drop 2 remaining)
             (conj ds [(first remaining) (second remaining)])))))

(defn problem32
  "
    We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once;
    for example, the 5-digit number, 15234, is 1 through 5 pandigital.

    The product 7254 is unusual, as the identity, 39 x 186 = 7254,
    containing multiplicand, multiplier, and product is 1 through 9 pandigital.

    Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
    HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum."
  []
  (let [pandigital-set (set (range 1 10))]
    (->> (range 1000 10000)
         (filter
          (fn [p]
            (let [p-digits (digits p)
                  p-digit-set (set p-digits)
                  ds (drop 1 ;; first divisor pair is always 1 and itself
                           (divisor-pairs p))]
              (and (= (count p-digits) (count p-digit-set))
                   (some (fn [[a b]]
                           (let [a-digits (digits a)
                                 b-digits (digits b)
                                 a-digit-set (set a-digits)
                                 b-digit-set (set b-digits)]
                             (and (= (count pandigital-set) (+ (count a-digits) (count b-digits) (count p-digits)))
                                  (= (union a-digit-set b-digit-set p-digit-set) pandigital-set))))
                         ds)))))
         (reduce +))))

(defn problem33
  "
    The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

    We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

    There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.

    If the product of these four fractions is given in its lowest common terms, find the value of the denominator."
  []
  (let [candidates (set (filter #(not= 0 (mod % 10))
                                (range 11 100)))]
    (->> (map (fn [n]
                (map (fn [d] [n d])
                     (disj candidates n)))
              candidates)
         (apply concat)
         (filter (fn [[n d]]
                   (let [n-digits (set (digits n))
                         d-digits (set (digits d))]
                     (and (< n d)
                          (= 2 (count n-digits))
                          (= 2 (count d-digits))
                          (= 1 (count (intersection n-digits d-digits)))
                          (= (/ n d)
                             (/ (first (difference n-digits d-digits))
                                (first (difference d-digits n-digits))))))))
         (map (fn [[n d]] (/ n d)))
         (reduce *))))

(defn problem34
  "
    145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

    Find the sum of all numbers which are equal to the sum of the factorial of their digits.

    Note: As 1! = 1 and 2! = 2 are not sums they are not included."
  []
  (->> (range 3 1000000)
       (filter #(= %
                   (->> (digits %)
                        (map factorial)
                        (reduce +))))
       (reduce +)))

(defn from-digits [ds]
  (let [s (apply str ds)]
    (if (< (count s) 10)
      (Integer/parseInt s)
      (bigint s))))

(defn rotations
  ([col] (rotations col 0 (count col)))
  ([col i c]
   (if (>= i c)
     nil
     (cons col
           (lazy-seq (rotations (concat (rest col) (list (first col)))
                                (inc i)
                                c))))))

(defn problem35
  "
    The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

    There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

    How many circular primes are there below one million?"
  []
  (->> (sieve-of-eratosthenes 1000000)
       (filter (fn [p]
                 (every? (fn [r]
                           (prime? (from-digits r)))
                         (rotations (digits p)))))
       (count)))

(defn binary-digits
  [n]
  (Integer/toString n 2))

(defn palindrome?
  [col]
  (let [c (count col)]
    (every? #(= (nth col %)
                (nth col (- c % 1)))
            (range (/ c 2)))))

(defn problem36
  "
    The decimal number, 585 = 1001001001_2 (binary), is palindromic in both bases.

    Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

    (Please note that the palindromic number, in either base, may not include leading zeros.)"
  []
  (->> (range 1000000)
       (filter #(and (palindrome? (digits %))
                     (palindrome? (binary-digits %))))
       (reduce +)))

(defn problem37
  "
    The number 3797 has an interesting property.
    Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7.
    Similarly we can work from right to left: 3797, 379, 37, and 3.

    Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

    NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes."
  []
  (let [primes (set (sieve-of-eratosthenes 1000000))]
    (->> (disj primes 2 3 5 7) ;; single digit is not considered truncatable prime
         (filter (fn [p]
                   (let [ds (digits p)]
                     (and
                      (loop [ds (rest ds)]
                        (cond
                          (empty? ds) true
                          (primes (from-digits ds)) (recur (rest ds))
                          :else false))
                      (loop [ds (take (dec (count ds)) ds)]
                        (cond
                          (empty? ds) true
                          (primes (from-digits ds)) (recur (take (dec (count ds)) ds))
                          :else false))))))
         (reduce +))))

(defn problem38
  "
    Take the number 192 and multiply it by each of 1, 2, and 3:
        begin{align}
        192 times 1 &= 192
        192 times 2 &= 384
        192 times 3 &= 576
        end{align}
    By concatenating each product we get the 1 to 9 pandigital, 192384576.
    We will call 192384576 the concatenated product of 192 and (1,2,3).

    The same can be achieved by starting with 9
    and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645,
    which is the concatenated product of 9 and (1,2,3,4,5).

    What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2,...,n) where n>1?"
  []
  (let [pandigital-set (set (range 1 10))]
    (->> (range 100000)
         (filter #(= (count (digits %))
                     (count (set (digits %)))))
         (map #(loop [ds (digits %)
                      i 2]
                 (let [new-ds (concat ds (digits (* i %)))]
                   (cond
                     (= 9 (count new-ds)) new-ds
                     (< (count new-ds) 9) (recur new-ds (inc i))
                     :else '(0)))))
         (filter #(= (set %)
                     pandigital-set))
         (map from-digits)
         (apply max))))

(defn problem39
  "
    If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p=120.
    {20,48,52}, {24,45,51}, {30,40,50}
    For which value of p<=1000, is the number of solutions maximised?"
  []
  (loop [p 120
         max-p 0
         max-count 0
         max-triangles []]
    (if (>= p 1000)
      {:p max-p :triangles max-triangles}
      (let [triangles (->> (range 1 (/ p 3))
                           (map (fn [a]
                                  (->> (range a (/ p 2))
                                          (map (fn [b]
                                                 [a b (- p a b)]))
                                          (filter (fn [[a b c]]
                                                    (= (* c c) (+ (* a a) (* b b)))))
                                          (first))))
                           (filter #(not (nil? %))))]
        (if (> (count triangles) max-count)
          (recur (inc p)
                 p
                 (count triangles)
                 triangles)
          (recur (inc p)
                 max-p
                 max-count
                 max-triangles))))))

(defn problem40
  "
    An irrational decimal fraction is created by concatenating the positive integers:
        0.12345678910{color{red}mathbf 1}112131415161718192021...
    It can be seen that the 12th digit of the fractional part is 1.

    If d_n
    represents the nth digit of the fractional part, find the value of the following expression.
        d_1 x d_{10} x d_{100} x d_{1000} x d_{10000} x d_{100000} x d_{1000000}
"
  []
  (letfn [(d ([] (d 1))
            ([i] (lazy-cat (digits i) (d (inc i)))))]
    (let [ds (d)]
      (->> (range 7)
           (map #(pow 10 %))
           (map #(nth ds (dec %)))
           (reduce *)))))

(defn problem41
  "We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.

  What is the largest n-digit pandigital prime that exists?"
  []
  (->> (reverse (range 1 10)) ;; start from 9-digit pandigital and go down
       (map (fn [n] (lexicographic-permutations (reverse (range 1 n))))) ;; permute the n-digit pandigitals from largest to smallest
       (apply concat) ;; concat the pandigital permutation seqs
       (map from-digits)
       (filter prime?)
       (first)))

(defn problem42
  "
    The nth term of the sequence of triangle numbers is given by, t_n = (1/2)n(n+1); so the first ten triangle numbers are:
        1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
    By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value.
    For example, the word value for SKY is 19 + 11 + 25 = 55 = t_{10}.
    If the word value is a triangle number then we shall call the word a triangle word.

    Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words, how many are triangle words?"
  []
  (let [input (slurp (io/resource "0042_words.txt"))
        words (clojure.string/split (clojure.string/replace input #"\"" "") #",")
        word-values (map word-value words)
        max-word-value (apply max word-values)
        triangle-set (set (take-while #(<= % max-word-value) (triangle-numbers)))]
    (count (filter triangle-set word-values))))

(defn big-from-digits
  [ds]
  (bigint (apply str ds)))

(defn problem43
  "
    The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order,
    but it also has a rather interesting sub-string divisibility property.

    Let d_1 be the 1st digit, d_2 be the 2nd digit, and so on. In this way, we note the following:
    - d_2d_3d_4=406 is divisible by 2
    - d_3d_4d_5=063 is divisible by 3
    - d_4d_5d_6=635 is divisible by 5
    - d_5d_6d_7=357 is divisible by 7
    - d_6d_7d_8=572 is divisible by 11
    - d_7d_8d_9=728 is divisible by 13
    - d_8d_9d_{10}=289 is divisible by 17
    Find the sum of all to pandigital numbers with this property."
  []
  (->> (lexicographic-permutations (range 10))
       (map vec)
       (filter (fn [d]
                 (and (= (mod (from-digits (subvec d 1 4)) 2) 0)
                      (= (mod (from-digits (subvec d 2 5)) 3) 0)
                      (= (mod (from-digits (subvec d 3 6)) 5) 0)
                      (= (mod (from-digits (subvec d 4 7)) 7) 0)
                      (= (mod (from-digits (subvec d 5 8)) 11) 0)
                      (= (mod (from-digits (subvec d 6 9)) 13) 0)
                      (= (mod (from-digits (subvec d 7 10)) 17) 0)
                      )))
       (map big-from-digits)
       (reduce +)))

(defn pentagonal-number
  [n]
  (/ (* n (- (* 3 n) 1))
     2))

(defn pentagonal-number?
  [n]
  (= (mod (sqrt (+ 1 (* 24 n)))
          6)
     5.0))

(defn pentagonal-numbers
  ([] (pentagonal-numbers 1))
  ([n] (cons (pentagonal-number n) (lazy-seq (pentagonal-numbers (inc n))))))

(defn problem44
  "
    Pentagonal numbers are generated by the formula, P_n=n(3n-1)/2. The first ten pentagonal numbers are:
        1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...
    It can be seen that P_4 + P_7 = 22 + 70 = 92 = P_8. However, their difference, 70 - 22 = 48, is not pentagonal.

    Find the pair of pentagonal numbers, P_j and P_k,
    for which their sum and difference are pentagonal and D = |P_k - P-j| is minimised; what is the value of D?"
  []
  (->> (iterate inc 2)
       (map (fn [k] (map (fn [j] [j k]) (range 1 k))))
       (map (fn [pairs]
              (map (fn [[j k]]
                     (let [pj (pentagonal-number j)
                           pk (pentagonal-number k)
                           s (+ pj pk)
                           d (- pk pj)]
                       [j k pj pk s d]))
                   pairs)))
       (map (fn [pairs]
                 (filter (fn [[j k pj pk s d]]
                           (and (pentagonal-number? s)
                                (pentagonal-number? d)))
                         pairs)))
       (filter #(not (empty? %)))
       (first)))

(defn hexagonal-number
  [n]
  (* n
     (- (* 2 n)
        1)))

(defn hexagonal-numbers
  ([] (hexagonal-numbers 1))
  ([n] (cons (hexagonal-number n) (lazy-seq (hexagonal-numbers (inc n))))))

(defn problem45
  "
    <p>Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:</p>
    <table>
    <tr><td>Triangle</td> <td> </td> <td>$T_n=n(n+1)/2$</td> <td> </td> <td>$1, 3, 6, 10, 15, ...$</td> </tr>
    <tr><td>Pentagonal</td> <td> </td> <td>$P_n=n(3n - 1)/2$</td> <td> </td> <td>$1, 5, 12, 22, 35, ...$</td> </tr>
    <tr><td>Hexagonal</td> <td> </td> <td>$H_n=n(2n - 1)$</td> <td> </td> <td>$1, 6, 15, 28, 45, ...$</td> </tr>
    </table>
    <p>It can be verified that $T_{285} = P_{165} = H_{143} = 40755$.</p>
    <p>Find the next triangle number that is also pentagonal and hexagonal.</p> "
  []
  (->> (hexagonal-numbers)
       (filter #(and (triangle-number? %)
                     (pentagonal-number? %)))
       (take 3)))

(defn problem46
  "
    It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.
        9 = 7 + 2 x 1^2
        15 = 7 + 2 x 2^2
        21 = 3 + 2 x 3^2
        25 = 7 + 2 x 3^2
        27 = 19 + 2 x 2^2
        33 = 31 + 2 x 1^2
    It turns out that the conjecture was false.

    What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?"
  []
  (let [primes (sieve-of-eratosthenes 1000000)]
    (->> (iterate #(+ 2 %) 1) ;; odd numbers
         (filter (fn [n] (not (prime? n)))) ;; only composites
         (map (fn [n]
                [n ;; keep the odd composite number around because we need it for the answer
                 (->> (take-while #(< % n) primes) ;; primes less than n
                      (filter (fn [p]
                                (= 0.0 ;; half of the difference of n and p is a perfect square
                                   (mod (sqrt (/ (- n p)
                                                 2))
                                        1))
                                )))]))
         (filter (fn [[n ps]] (empty? ps))) ;; find the odd composite where no primes work
         (first)
         )))

(defn prime-factorization
  "Returns the unique prime factorization of n in the form [[2 a] [3 b] ...] where a, b, etc. are not 0"
  [n]
  (let [limit (inc (/ n 2))]
    (when (< (:limit @prime-cache) limit)
      (reset! prime-cache {:limit (* 2 limit) :primes (sieve-of-eratosthenes (* 2 limit))}))
    (->> (take-while #(< % limit) (:primes @prime-cache)) ;; foreach prime less than limit
         (map (fn [p] ;; how many times does p evenly divide n?
                [p
                 (loop [e 0
                        r n]
                   (if (= (mod r p) 0)
                     (recur (inc e)
                            (/ r p))
                     e))]))
         (filter (fn [[p e]] (not (= e 0)))) ;; remove 0 exponents
         (#(if (empty? %)
             (list [n 1]) ;; n is prime
             %))
     )))

(defn from-factorization
  [f]
  (->> f
       (map #(apply pow %))
       (reduce *)
       (int)))

(defn problem47
  "
    The first two consecutive numbers to have two distinct prime factors are:
        14 = 2 x 7
        15 = 3 x 5.
    The first three consecutive numbers to have three distinct prime factors are:
        644 = 2^2 x 7 x 23
        645 = 3 x 5 x 43
        646 = 2 x 17 x 19.
    Find the first four consecutive integers to have four distinct prime factors each. What is the first of these numbers?"
  ([] (problem47 4)) ;; allow testing with n=2 and n=3
  ([n]
   ;; ;; Slow! Factorizing takes a while
   ;; (let [factorizations (->> (iterate inc 2) ;; factorize all natural numbers once
   ;;                           (map prime-factorization))]
   ;;   (loop [fs factorizations]
   ;;     (let [consecutive-facts (take n fs)] ;; group n consecutive numbers post-factorization
   ;;       (if (and (every? #(= n (count %)) ;; all n numbers have n prime factors
   ;;                        consecutive-facts)
   ;;                ;; do we need to check for distinct factors?
   ;;                ;; (empty? (apply intersection (map set consecutive-facts))) ;; all prime factors are distinct
   ;;                )
   ;;         (map from-factorization consecutive-facts) ;; We got it!
   ;;         (recur (rest fs)) ;; try the next n consecutive factorizations
   ;;         ))))
   (let [limit (pow 2 18)
         factor-counts-t (transient (vec (repeat limit 0)))]
     (for [i (range 2 limit)]
       (when (= (nth factor-counts-t i) 0) ;; i is prime (hasn't had a factor yet)
         (for [n (range i limit i)] ;; for each multiple of i below limit
           (assoc! factor-counts-t n (inc (nth factor-counts-t n))) ;; increment factor count
           )))
     (let [factor-counts (persistent! (reduce (fn [factor-counts i]
                                                (if (= (nth factor-counts i) 0) ;; i is prime (hasn't had a factor yet)
                                                  (reduce (fn [factor-counts n]
                                                            (assoc! factor-counts n (inc (nth factor-counts n)))) ;; increment factor count
                                                          factor-counts ;; modifiy the transient vec
                                                          (range i limit i) ;; for each multiple of i below limit
                                                          )
                                                  factor-counts ;; move on, i isn't prime
                                                  ))
                                              (transient (vec (repeat limit 0)))
                                              (range 2 limit)))]
        (loop [i 2]
          (if (every? #(= % n)
                      (subvec factor-counts i (+ i n)))
            i
            (recur (inc i)))))
     )
   ))

(defn problem48
  "
    The series, 1^1 + 2^2 + 3^3 + ... + 10^{10} = 10405071317.

    Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^{1000}."
  ([] (problem48 1000))
  ([n]
   (as-> (range 1 (inc n)) $
        (map bigint $)
        (map #(reduce * (repeat % %)) $)
        (reduce + $)
        (digits $)
        (vec $)
        (subvec $ (- (count $) 10))
        (apply str $))
   )
  )

(defn problem49
  "
    The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways:
    (i) each of the three terms are prime, and,
    (ii) each of the 4-digit numbers are permutations of one another.

    There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property,
    but there is one other 4-digit increasing sequence.

    What 4-digit number do you form by concatenating the three terms in this sequence?"
  []
  (let [candidates (set (drop-while #(< % 1000) (sieve-of-eratosthenes 10000))) ;; 4-digit primes
        ]
    (for [p1 candidates
          p2 candidates
          :let [p3 (+ p2 (- p2 p1))
                digit-set (set (digits p1))]
          :when (and (< p1 p2 p3)
                     (candidates p3) ;; p3 is also a 4-digit prime
                     (apply = (map #(set (digits %)) [p1 p2 p3])))]
      [p1 p2 p3 (str p1 p2 p3)])
    )
  )

(defn problem50
  "
    The prime 41, can be written as the sum of six consecutive primes:
        41 = 2 + 3 + 5 + 7 + 11 + 13.
    This is the longest sum of consecutive primes that adds to a prime below one-hundred.
    The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
    Which prime, below one-million, can be written as the sum of the most consecutive primes?"
  []
  (let [limit 1000000
        primes (sieve-of-eratosthenes limit)]
    (loop [max-prime 5
           max-len 2
           primes-remaining primes]
      (if (empty? primes-remaining)
        [max-prime max-len]
        (let [[len p] (loop [l 1
                             prime-l 1
                             p (first primes-remaining)
                             s (first primes-remaining)
                             primes-remaining (rest primes-remaining)
                             ]
                        (if (empty? primes-remaining)
                          [prime-l p]
                          (let [next-s (+ s (first primes-remaining))
                                next-l (inc l)]
                            (if (> s limit)
                              [prime-l p]
                              (recur (inc l)
                                     (if (primes next-s) ;; the sum is prime
                                       next-l
                                       prime-l)
                                     (if (primes next-s) ;; the sum is prime
                                       next-s
                                       p)
                                     next-s
                                     (rest primes-remaining)
                                     )
                              ))))]
          (if (> len max-len)
            (recur p
                   len
                   (rest primes-remaining))
            (recur max-prime
                   max-len
                   (rest primes-remaining)))
          )))))

(defn matching-digit-index-sets [n]
  (let [ds (digits n)]
    (->> (range (count ds))
      (combinatorics/subsets)
      (remove empty?)
      (filter #(apply = (map (fn [i] (nth ds i)) %))))))

(defn problem51
  "
    By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.

    By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers,
    yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.

    Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.
  "
  ([] (problem51 8))
  ([n]
   (let [primes (sieve-of-eratosthenes 1000000)]
     (->> primes
          (map (fn [p]
                    (let [ds (vec (digits p))
                          indices (matching-digit-index-sets p)
                          prime-digit-replacements (->> indices
                                                        (map (fn [is]
                                                               (->> (range 10)
                                                                    (map (fn [d]
                                                                           (reduce (fn [ds i] (assoc ds i d))
                                                                                   ds
                                                                                   is)))
                                                                    (filter #(and (primes (from-digits %))
                                                                                  (= (count (digits (from-digits %)))
                                                                                     (count ds)))))))
                                                        (filter #(>= (count %) n)))]
                      [p prime-digit-replacements])))
          (filter (fn [[p ps]]
                    (not (empty? ps))))
          (first)))))

(defn problem52
  "It can be seen that the number, 125874, and its double, 251748 , contain exactly the same digits, but in a different order.

   Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits."
  []
  (->> (range 1 1000000000)
       (filter (fn [n]
                 (let [ds (digits n)
                       d-set (set ds)]
                   (->> (range 2 7)
                    (map (fn [m] (* m n)))
                    (filter #(let [p-ds (digits %)]
                               (and (empty? (difference d-set (set p-ds)))
                                    (= (count p-ds) (count ds)))))
                    (#(= 5 (count %)))))))
       (first)))

(defn problem53
  "
  There are exactly ten ways of selecting three from five, 12345:
    123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
  In combinatorics, we use the notation, \\binom 5 3 = 10.

  In general, \\binom n r = \\dfrac{n!}{r!(n-r)!}, where r<=n, n!=n*(n-1)*...*3*2*1, and 0! = 1.

  It is not until n=23, that a value exceeds one-million: \\binom {23} {10} = 1144066.

  How many, not necessarily distinct, values of \\binom n r for 1<=n<=100, are greater than one-million?
  "
  []
  (->> (range 1 101)
       (map (fn [n]
              (->> (range 1 (inc n))
                   (map (fn [r] (choose n r)))
                   (filter #(> % 1000000)))))
       (reduce concat)
       (count)
       ))

(def ranks (mapv str [2 3 4 5 6 7 8 9 "T" "J" "Q" "K" "A"]))
(def suits ["C" "D" "H" "S"])

(defn parse-card
  "s should be a two character string.
  The first character is the rank of the card: 2-9, T (10), J (Jack), Q (Queen), K (King), or A (Ace).
  The second character is the suit: C (Clubs), D (Diamonds), H (Hearts), S (Spades)
  "
  [s]
  {:rank (subs s 0 1)
   :suit (subs s 1)})

(defn compare-ranks [c1 c2]
  (compare (.indexOf ranks (:rank c1))
           (.indexOf ranks (:rank c2))))

;; Hand predicates return the subset of the hand used given a parsed rank-sorted hand.
(defn royal-flush?
  "Returns h when h (for 'hand') is a royal flush, nil otherwise. h should be a col of parsed cards sorted by rank"
  [h]
  (when (and (apply = (map :suit h))
             (= (map :rank h) ["T" "J" "Q" "K" "A"]))
    h))

(defn straight?
  [h]
  (when (or (= (->> h
                   (map :rank)
                   (map #(.indexOf ranks %)))
              (range (.indexOf ranks (:rank (first h)))
                     (+ 5 (.indexOf ranks (:rank (first h))))))
            (= (map :rank h) ["2" "3" "4" "5" "A"]))
    h))

(defn flush?
  [h]
  (when (apply = (map :suit h))
    h))

(defn straight-flush?
  "Returns h when h is a straight flush, nil otherwise"
  [h]
  (when (and (straight? h)
             (flush? h))
    h))

(defn four-of-a-kind? [h]
  (if (apply = (map #(:rank (nth h %))
                    (range 0 4)))
    (subvec (vec h) 0 4)
    (when (apply = (map #(:rank (nth h %))
                      (range 1 5)))
      (subvec (vec h) 1 5))))

(defn full-house? [h]
  (when (or (and (apply = (map #(:rank (nth h %)) (range 0 3)))
                 (apply = (map #(:rank (nth h %)) (range 3 5))))
            (and (apply = (map #(:rank (nth h %)) (range 0 2)))
                 (apply = (map #(:rank (nth h %)) (range 2 5)))))
    h))

(defn three-of-a-kind? [h]
  (let [start (->> (range 3)
                   (filter (fn [start]
                             (apply = (map #(:rank (nth h %)) (range start (+ start 3))))))
                   (first))]
    (when start
      (subvec (vec h) start (+ start 3)))))

(defn two-pair? [h]
  (if (= (:rank (nth h 0))
         (:rank (nth h 1)))
    (if (= (:rank (nth h 2))
           (:rank (nth h 3)))
      (take 4 h)
      (when (= (:rank (nth h 3))
               (:rank (nth h 4)))
        (concat (take 2 h) (drop 3 h))))
    (when (and (= (:rank (nth h 1))
                  (:rank (nth h 2)))
               (= (:rank (nth h 3))
                  (:rank (nth h 4))))
      (drop 1 h))))

(defn pair? [h]
  (let [start (->> (range 4)
                   (filter (fn [start]
                             (apply = (map #(:rank (nth h %)) (range start (+ start 2))))))
                   (first))]
    (when start
      (subvec (vec h) start (+ start 2)))))

(defn high-card? [h]
  (list (last h)))

(def hand-predicates [high-card? pair? two-pair? three-of-a-kind? straight? flush? full-house? four-of-a-kind? straight-flush? royal-flush?])

(defn best-hand-index [h]
  (->> (range (count hand-predicates))
       (reverse)
       (map (fn [i] [i ((nth hand-predicates i) h)]))
       (filter #(not (nil? (second %))))
       (first)
       ((fn [[i cs]]
          ;; cs is the subset of h making up the best poker hand.
          ;; Third entry of returned vec is the kickers
          [i cs (reduce (fn [h c] (remove #(= % c) h))
                        h
                        cs)]))))

(defn compare-hands [h1 h2]
  (let [[i1 cs1 ks1] (best-hand-index h1)
        [i2 cs2 ks2] (best-hand-index h2)]
    (if (= i1 i2)
      (if (= i1 (.indexOf hand-predicates full-house?))
        ;; full house ties need to be handled specially, because it cares about the three-of-a-kind, not highest card
        ;; The two three-of-a-kinds cannot be equal (unless someone has an ace up their sleeve)
        (let [t1 (three-of-a-kind? h1)
              t2 (three-of-a-kind? h2)]
          (compare-ranks (first t1) (first t2)))
        (let [comp-c (compare-ranks (last cs1)
                                    (last cs2))]
            (if (= comp-c 0)
            (first (drop-while #(= % 0) ;; drop equal ranks
                                (map compare-ranks (reverse ks1) (reverse ks2)))) ;; compare kickers highest to lowest
            comp-c)))
      (compare i1 i2))))

(defn problem54
  "
  In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:

    High Card: Highest value card.
    One Pair: Two cards of the same value.
    Two Pairs: Two different pairs.
    Three of a Kind: Three cards of the same value.
    Straight: All cards are consecutive values.
    Flush: All cards of the same suit.
    Full House: Three of a kind and a pair.
    Four of a Kind: Four cards of the same value.
    Straight Flush: All cards are consecutive values of same suit.
    Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.

  The cards are valued in the order:
  2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

  If two players have the same ranked hands then the rank made up of the highest value wins;
  for example, a pair of eights beats a pair of fives (see example 1 below).
  But if two ranks tie, for example, both players have a pair of queens, then highest cards in each hand are compared (see example 4 below);
  if the highest cards tie then the next highest cards are compared, and so on.

  Consider the following five hands dealt to two players:
  Hand | Player 1         | Player 2           | Winner
  1    | 5H 5C 6S 7S KD   | 2C 3S 8S 8D TD     | Player 2
         Pair of Fives      Pair of Eights
  2    | 5D 8C 9S JS AC   | 2C 5C 7D 8S QH     | Player 1
         Highest card Ace   Highest card Queen
  3    | 2D 9C AS AH AC   | 3D 6D 7D TD QD     | Player 2
         Three Aces         Flush with Diamonds
  4    | 4D 6S 9H QH QC   | 3D 6D 7H QD QS     | Player 1
         Pair of Queens     Pair of Queens
         Highest card Nine  Highest card Seven
  5    | 2H 2D 4C 4D 4S   | 3C 3D 3S 9S 9D     | Player 1
         Full House         Full House
         With Three Fours   with Three Threes

  The file, poker.txt, contains one-thousand random hands dealt to two players.
  Each line of the file contains ten cards (separated by a single space):
  the first five are Player 1's cards and the last five are Player 2's cards.
  You can assume that all hands are valid (no invalid characters or repeated cards),
  each player's hand is in no specific order,
  and in each hand there is a clear winner.

  How many hands does Player 1 win?
  "
  []
  (let [input (slurp (io/resource "0054_poker.txt"))
        lines (clojure.string/split-lines input)
        hands (map (fn [l]
                     (let [cards (clojure.string/split l #" ")]
                       [(take 5 cards) (drop 5 cards)]))
                   lines)]
    (->> hands
         (map (fn [hs] (map (fn [h] (sort compare-ranks (map parse-card h))) hs)))
         (filter (fn [[h1 h2]]
                   (> (compare-hands h1 h2)
                      0)))
         (count))))

(defn problem55
  "
  If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.

  Not all numbers produce palindromes so quickly. For example,
    \\begin{align}
    349 + 943 &= 1292\\
    1292 + 2921 &= 4213\\
    4213 + 3124 &= 7337
    \\end{align}
  That is, 394 took three iterations to arrive at a palindrome.

  Although no one has proved it yet, it is thought that some numbers, like 196, never produce a palindrome.
  A number that never forms a palindrome through the reverse and add process is called a Lychrel number.
  Due to the theoretical nature of these numbers, and for the purpose of this problem, we shall assume that a number is Lychrel until proven otherwise.
  In addition you are given that for every number below ten-thousand, it will either
  (i) become a palindrome in less than fifty iterations, or,
  (ii) no one, with all the computing power that exists, has managed so far to map it to a palindrome.
  In fact, 10677 is the first number to be shown to require over fifty iterations before producing a palindrome: 4668731596684224866951378664 (53 iterations, 28-digits).

  Surprisingly, there are palindromic numbers that are themselves Lychrel numbers; the first example is 4994.

  How many Lychrel numbers are there below ten-thousand?

  NOTE: Wording was modified slightly on 24 April 2007 to emphasise the theoretical nature of Lychrel numbers."
  []
  (->> (range 10000 0 -1)
       (filter (fn [n]
                 (loop [n n
                        i 0]
                   (if (< i 50)
                     (let [s (+ n (from-digits (reverse (digits n))))]
                       (if (palindrome-number? s)
                         false
                         (recur s (inc i))))
                     true))))
       (count)))

(defn problem56
  "A googol (10^100) is a massive number: one followed by one-hundred zeros;
  100^100  is almost unimaginably large: one followed by two-hundred zeros.
  Despite their size, the sum of the digits in each number is only 1.

  Considering natural numbers of the form, a^b, where a,b<100, what is the maximum digital sum?"
  []
  (->> (for [a (range 100)
             b (range 100)]
             [a b (.pow (bigdec a) b)])
       (map (fn [[a b p]] [a b (digits p)]))
       (map (fn [[a b ds]] [a b (reduce + ds)]))
       (reduce (fn [[maxa maxb maxdsum] [a b dsum]]
                 (if (> dsum maxdsum)
                   [a b dsum]
                   [maxa maxb maxdsum])))))

(defn root-2-continued-fraction-sequence
  ([] (root-2-continued-fraction-sequence 0))
  ([frac]
   (let [frac (/ 1 (+ 2 frac))]
     (cons (+ 1 frac)
           (lazy-seq (root-2-continued-fraction-sequence frac))))))

(defn problem57
  "It is possible to show that the square root of two can be expressed as an infinite continued fraction.
    \\sqrt 2 =1+ \\frac 1 {2+ \\frac 1 {2 +\\frac 1 {2+ \\dots}}}
   By expanding this for the first four iterations, we get:
    1 + \\frac 1 2 = \\frac  32 = 1.5
    1 + \\frac 1 {2 + \\frac 1 2} = \\frac 7 5 = 1.4
    1 + \\frac 1 {2 + \\frac 1 {2+\\frac 1 2}} = \\frac {17}{12} = 1.41666 \\dots
    1 + \\frac 1 {2 + \\frac 1 {2+\\frac 1 {2+\\frac 1 2}}} = \\frac {41}{29} = 1.41379 \\dots
   The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985,
   is the first example where the number of digits in the numerator exceeds the number of digits in the denominator.

   In the first one-thousand expansions, how many fractions contain a numerator with more digits than the denominator?"
  []
  (->> (root-2-continued-fraction-sequence)
       (take 1000)
       (filter #(> (count (digits (numerator %)))
                   (count (digits (denominator %)))))
       (count)))

(defn problem58
  "Starting with and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.

        37 36 35 34 33 32 31
        38 17 16 15 14 13 30
        39 18  5  4  3 12 29
        40 19  6  1  2 11 28
        41 20  7  8  9 10 27
        42 21 22 23 24 25 26
        43 44 45 46 47 48 49

  It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting is that
  out of the numbers lying along both diagonals are prime; that is, a ratio of 8/13 ~= 62%.

  If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed.
  If this process is continued, what is the side length of the square spiral for which the ratio of primes along both diagonals first falls below 10%?"
  []
  (loop [l 7
         diag-count 13
         prime-count 8]
    (if (< (/ prime-count diag-count) 0.1)
      l
      (let [new-l (+ l 2)
            new-diags (spiral-diagonals new-l)
            new-primes-count (count (filter prime? new-diags))]
        (recur new-l
               (+ diag-count 4)
               (+ prime-count new-primes-count))))))

(defn problem59
  "
  Each character on a computer is assigned a unique code and the preferred standard is ASCII (American Standard Code for Information Interchange). For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.

  A modern encryption method is to take a text file, convert the bytes to ASCII, then XOR each byte with a given value, taken from a secret key.
  The advantage with the XOR function is that using the same encryption key on the cipher text, restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.

  For unbreakable encryption, the key is the same length as the plain text message, and the key is made up of random bytes.
  The user would keep the encrypted message and the encryption key in different locations, and without both \"halves\", it is impossible to decrypt the message.

  Unfortunately, this method is impractical for most users, so the modified method is to use a password as a key.
  If the password is shorter than the message, which is likely, the key is repeated cyclically throughout the message.
  The balance for this method is using a sufficiently long password key for security, but short enough to be memorable.

  Your task has been made easy, as the encryption key consists of three lower case characters.
  Using 0059_cipher.txt (right click and 'Save Link/Target As...'), a file containing the encrypted ASCII codes,
  and the knowledge that the plain text must contain common English words,
  decrypt the message and find the sum of the ASCII values in the original text.
  "
  []
  (let [cipher-text (slurp (io/resource "0059_cipher.txt"))
        cipher-chars (map Integer/parseInt (clojure.string/split cipher-text #","))
        cipher-len (count cipher-chars)
        ascii-freq (->> (io/resource "ascii_freq.txt")
                        (slurp)
                        (clojure.string/split-lines)
                        (map (fn [line]
                               (let [[char-num freq] (clojure.string/split line #":")]
                                 [(Integer/parseInt char-num) (Float/parseFloat freq)])))
                        (into {}))
        key-char-candidates (range (int \a) (inc (int \z)))
        plain-texts (for [k1 key-char-candidates
                          k2 key-char-candidates
                          k3 key-char-candidates
                          :let [k (cycle [k1 k2 k3])]]
                      {:k [k1 k2 k3] :plaintext (map bit-xor cipher-chars k)})]
    (loop [plain-texts plain-texts]
      (let [{k :k plain-text :plaintext} (first plain-texts)
            plaintext-str (apply str (map char plain-text))]
        (if (clojure.string/includes? plaintext-str " the ")
          {:k k
           :char-sum (reduce + plain-text)
           :plaintext plaintext-str}
          (recur (rest plain-texts)))))))

(defn problem60
  "
  The primes 3, 7, 109, and 673, are quite remarkable.
  By taking any two primes and concatenating them in any order the result will always be prime.
  For example, taking 7 and 109, both 7109 and 1097 are prime.
  The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.

  Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.
  "
  ([]
   (let [primes (sieve-of-eratosthenes 10000)
         prime-candidates (take-while #(< % 10000) (drop-while #(< % 3) primes)) ;; 2 obviously won't have any pairs
         pairs (->> prime-candidates
                    (map (fn [a]
                           [a (apply sorted-set
                                     a
                                     (filter (fn [b]
                                               (and (not= a b)
                                                    (prime? (from-digits (concat (digits a) (digits b))))
                                                    (prime? (from-digits (concat (digits b) (digits a))))))
                                             prime-candidates))]))
                    (filter (fn [[p ps]] (>= (count ps) 5)))
                    (into (sorted-map)))]
     (set (for [[p1 p1-pairs] pairs
                p2 p1-pairs
                :when (not= p1 p2)
                :let [p1-p2-pairs (intersection p1-pairs (pairs p2))]
                :when (>= (count p1-p2-pairs) 5)
                p3 p1-p2-pairs
                :when (and (not= p1 p3)
                           (not= p2 p3))
                :let [p1-p2-p3-pairs (intersection p1-p2-pairs (pairs p3))]
                :when (>= (count p1-p2-p3-pairs) 5)
                p4 p1-p2-p3-pairs
                :when (and (not= p1 p4)
                           (not= p2 p4)
                           (not= p3 p4))
                :let [intersection-set (intersection p1-p2-p3-pairs (pairs p4))]
                :when (>= (count intersection-set) 5)
                ]
            {:set intersection-set
             :sum (reduce + (take 5 intersection-set))}))
     )))

(defn square-number? [n] (= (mod (sqrt n) 1) 0.0))
(defn hexagonal-number?
  [n]
  (= (mod (/ (+ 1 (sqrt (+ 1 (* 8 n))))
             4)
          1)
     0.0))
(defn heptagonal-number?
  [n]
 (= (mod (/ (+ 3 (sqrt (+ 9 (* 40 n))))
            10)
         1)
    0.0))

(defn octagonal-number?
  [n]
  (= (mod (/ (+ 1 (sqrt (+ 1 (* 3 n))))
             3)
          1)
     0.0))

(def polygonal-predicates
  [triangle-number? square-number? pentagonal-number? hexagonal-number? heptagonal-number? octagonal-number?])

(defn some-pred
  "Like some, but returns the first predicate which returns a logical true value, not the value."
  [preds x]
  (let [p (first preds)]
    (if p
      (let [px (p x)]
        (if px
          p
          (recur (rest preds) x)))
      nil)))

(defn problem61
  "
  Triangle, square, pentagonal, hexagonal, heptagonal, and octagonal numbers are all figurate (polygonal) numbers and are generated by the following formulae:
  Triangle          P_{3,n}=n(n+1)/2            1, 3, 6, 10, 15, \\dots
  Square            P_{4,n}=n^2                 1, 4, 9, 16, 25, \\dots
  Pentagonal        P_{5,n}=n(3n-1)/2           1, 5, 12, 22, 35, \\dots
  Hexagonal         P_{6,n}=n(2n-1)             1, 6, 15, 28, 45, \\dots
  Heptagonal        P_{7,n}=n(5n-3)/2           1, 7, 18, 34, 55, \\dots
  Octagonal         P_{8,n}=n(3n-2)             1, 8, 21, 40, 65, \\dots

  The ordered set of three -digit numbers: 8128, 2882, 8281, has three interesting properties.

   - The set is cyclic, in that the last two digits of each number is the first two digits of the next number (including the last number with the first).
   - Each polygonal type: triangle (P_{3,127}=8128), square (P_{4,91}=8281), and pentagonal (P_{5,44}=2882), is represented by a different number in the set.

  This is the only set of 4-digit numbers with this property.

  Find the sum of the only ordered set of six cyclic 4-digit numbers for which each polygonal type: triangle, square, pentagonal, hexagonal, heptagonal, and octagonal, is represented by a different number in the set.
  "
  []
  (seq
   (for [a (range 1 10)
         b (range 10)
         c (range 1 10)
         d (range 10)
         :let [n1 (from-digits [a b c d])
               pred1 (some-pred polygonal-predicates n1)
               remaining-predicates (remove #{pred1} polygonal-predicates)]
         :when pred1
         e (range 1 10)
         f (range 10)
         :let [n2 (from-digits [c d e f])
               pred2 (some-pred remaining-predicates n2)
               remaining-predicates (remove #{pred2} remaining-predicates)]
         :when pred2
         g (range 1 10)
         h (range 10)
         :let [n3 (from-digits [e f g h])
               pred3 (some-pred remaining-predicates n3)
               remaining-predicates (remove #{pred3} remaining-predicates)]
         :when pred3
         i (range 1 10)
         j (range 10)
         :let [n4 (from-digits [g h i j])
               pred4 (some-pred remaining-predicates n4)
               remaining-predicates (remove #{pred4} remaining-predicates)]
         :when pred4
         k (range 1 10)
         l (range 10)
         :let [n5 (from-digits [i j k l])
               pred5 (some-pred remaining-predicates n5)
               remaining-predicates (remove #{pred5} remaining-predicates)]
         :when pred5
         :let [n6 (from-digits [k l a b])]
         :when (some-pred remaining-predicates n6)
         :let [ns [n1 n2 n3 n4 n5 n6]]
         ]
     {:set ns
      :sum (reduce + ns)
      :predicate-order [pred1 pred2 pred3 pred4 pred5 (first remaining-predicates)]})))

(defn permutations
  [col]
  (if (empty? col)
    [[]]
    (loop [passed []
           x (first col)
           remaining (rest col)
           perms []]
      (if (nil? x)
        perms
        (recur (conj passed x)
               (first remaining)
               (rest remaining)
               (concat perms
                       (map #(cons x %)
                            (permutations (concat passed remaining)))))))))

(defn problem62
  "
  The cube, 41063625 (345^3), can be permuted to produce two other cubes: 56623104 (384^3) and 66430125 (405^3).
  In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.
  Find the smallest cube for which exactly five permutations of its digits are cube.
  "
  []
  (time
   (let [cubes (map #(* % % %) (range 10000))
         sorted-digits (map (fn [n]
                              (let [ds (digits n)]
                                [n (sort ds)]))
                            cubes)
         digits-to-cubes (reduce (fn [dict [n ds]]
                                   (update dict ds #(conj (or % []) n)))
                                 {}
                                 sorted-digits)]
     (->> digits-to-cubes
          (vals)
          (filter #(= (count %) 5))
          (map #(apply min %))
          (apply min)))))

(defn is-nth-power?
  [a n]
  (= (int (pow (round (pow a (/ 1 n)))
               n))
     a))

(defn problem63
  "
  The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the 9-digit number, 134217728=8^9, is a ninth power.
  How many n-digit positive integers exist which are also an nth power?
  "
  []
  ;; No 22 digit numbers are 22nd powers. 9^22 has 21 digits, 10^22 has 23.
  ;; Thus the upper bound to check is 22nd powers, and the root is always single digit (10 ^ 2 is 3-digit)
  (count
   (for [a (range 1 10)
         n (range 22)
         :let [an (bigint (pow a n))]
         :when (= n (count (digits an)))]
     (str a "^" n "=" an)))
  )

(defn round-to-precision
  [precision n]
  (let [factor (pow 10 precision)]
    (/ (round (* n factor)) factor)))

(defn continued-fraction
  "Returns a fixed prefix and cycle of terms"
  ([r] (continued-fraction r 0 {} []))
  ([r idx f-to-idx terms]
   (let [i (int r)
         precision 8
         f (- r i)
         f-rounded (round-to-precision precision f)
         terms (conj terms i)]
     (if (= (float f) 0.0)
       {:fixed terms
        :cycle []}
       (if (contains? f-to-idx f-rounded)
         {:fixed (take (f-to-idx f-rounded) terms)
          :cycle (drop (f-to-idx f-rounded) terms)}
         (recur (/ 1 f) (inc idx) (assoc f-to-idx f-rounded (inc idx)) terms))))
   ))

(defn sqrt-continued-fraction
  [n]
  ;; (sqrt n) = a_0 + (sqrt n) - a_0
  ;;          = a_0 + (/ 1
  ;;                     (/ 1
  ;;                        (- (sqrt n) a_0)))
  ;;          = a_0 + (/ 1
  ;;                     (* (/ 1
  ;;                           (- (sqrt n) a_0))
  ;;                        (/ (+ (sqrt n) a_0)
  ;;                           (+ (sqrt n) a_0))))
  ;;          = a_0 + (/ 1
  ;;                     (/ (+ (sqrt n) a_0)
  ;;                        (- n (* a_0 a_0))))
  ;;          = a_0 + (/ 1
  ;;                     (/ (+ (sqrt n) a_0)
  ;;                        c_1))
  ;;          = a_0 + (/ 1
  ;;                     (+ a_1 (/ (+ (sqrt n) (- a_0 (* a_1 c_1)))
  ;;                               c_1)))
  ;;          = a_0 + (/ 1
  ;;                     (+ a_1 (/ (- (sqrt n) (- (* a_1 c_1) a_0))
  ;;                               c_1)))
  ;;          = a_0 + (/ 1
  ;;                     (+ a_1 (/ (- (sqrt n) b_1)
  ;;                               c_1)))
  ;;          = a_0 + (/ 1
  ;;                     (+ a_1 (/ 1
  ;;                               (/ c_1
  ;;                                  (- (sqrt n) b_1)))))
  ;;          = a_0 + (/ 1
  ;;                     (+ a_1 (/ 1
  ;;                               (/ (* c_1 (+ (sqrt n) b_1))
  ;;                                  (- n b_1^2)))))
  ;;          = a_0 + (/ 1
  ;;                     (+ a_1 (/ 1
  ;;                               (/ (+ (sqrt n) b_1)
  ;;                                  (/ (- n b_1^2) c_1)))))
  ;;          = a_0 + (/ 1
  ;;                     (+ a_1 (/ 1
  ;;                               (+ a_2 (/ (- (+ (sqrt n) b_1) (* a_2 (/ (- n b_1^2) c_1)))
  ;;                                         (/ (- n b_1^2) c_1))))))
  ;;          = a_0 + (/ 1
  ;;                     (+ a_1 (/ 1
  ;;                               (+ a_2 (/ (- (sqrt n) (- (* a_2 (/ (- n b_1^2) c_1)) b_1))
  ;;                                         (/ (- n b_1^2) c_1))))))
  ;;          = a_0 + (/ 1
  ;;                     (+ a_1 (/ 1
  ;;                               (+ a_2 (/ (- (sqrt n) b_2)
  ;;                                         c_2)))))
  ;; b_1 = (- (* a_1 c_1) a_0)
  ;; c_1 = (- n (* a_0 a_0))
  ;; b_2 = (- (* a_2 c_2) b_1)
  ;; c_2 = (/ (- n b_1^2)
  ;;          c_1)
  ;; a_n = (int (/ (+ (sqrt n) b_{n-1})
  ;;               c_n)
  ;; b_n = (- (* a_n c_n) b_{n-1})
  ;; c_n = (/ (- n (* b_{n-1} b_{n-1}))
  ;;          c_{n-1})
  ;; a_0 = (int (sqrt n))
  ;; b_0 = a_0
  ;; c_0 = 1
  (let [a_0 (int (sqrt n))
        b_0 a_0
        c_0 1]
    (if (= (* a_0 a_0) n)
      {:fixed [a_0]
       :cycle []}
      (loop [a a_0
             b a_0
             c 1
             f-to-idx {} ;; map from (a,b,c) to encountered index, where a, b, and c are the portions of (+ a (/ (- (sqrt n) b) c))
             terms []    ;; sequence a_0, a_1, etc, where each is the integer part of r at that step
             ]
        (let [terms (conj terms a)
              c (/ (- n (* b b))
                   c)]
          (if (= c 0)
            {:fixed terms
             :cycle []}
            (let [a (int (/ (+ (sqrt n) b)
                            c))
                  b (- (* a c) b)
                  cycle-start (f-to-idx [a b c])]
              (if (not (nil? cycle-start))
                {:fixed (take cycle-start terms)
                 :cycle (drop cycle-start terms)}
                (recur a b c
                       (assoc f-to-idx [a b c] (count terms))
                       terms)))))))))

(defn problem64
  "How many continued fractions for (sqrt n) for n <= 10,000 have an odd period?"
  ([] (problem64 10000))
  ([n]
   (->> (range (inc n))
        (map (fn [n] [n (sqrt-continued-fraction n)]))
        (map (fn [[n c]] [n (assoc c :period (count (:cycle c)))]))
        (filter (fn [[n {:keys [cycle]}]]
                  (not= (mod (count cycle)
                             2)
                        0)))
        (count)
        ;; for inspection:
        ;; (sort-by #(:period (second %)))
        ;; (reverse)
        ;; (first)
        )))

(defn e-continued-fraction-terms
  ([] (cons 2 (e-continued-fraction-terms 1)))
  ([k] (lazy-cat [1 (* 2 k) 1] (e-continued-fraction-terms (inc k)))))

(defn continued-fraction-convergent
  [terms n]
  (if (:fixed terms)
    ;; Turn {:fixed [...] :cycle [...]} into a single infinite sequence
    (recur (concat (:fixed terms)
                   (if (empty? (:cycle terms))
                     []
                     (cycle (:cycle terms))))
           n)
    (let [terms (reverse (take n terms))]
      (loop [res (first terms)
             terms (rest terms)]
        (if (empty? terms)
          res
          (recur (+ (first terms)
                    (/ 1 res))
                 (rest terms)))))))

(defn problem65
  "
  What is most surprising is that the important mathematical constant,
    e = [2; 1, 2, 1, 1, 4, 1, 1, 6, 1, ... , 1, 2k, 1, ...]
  Find the sum of digits in the numerator of the 100th convergent of the continued fraction for e.
  "
  []
  (->> (continued-fraction-convergent (e-continued-fraction-terms) 100)
       (numerator)
       (digits)
       (reduce +)))

(defn pell-equation-fundamental-solution
  "The fundamental solution [x y] for x^2 - D*y^2 = 1"
  ;; see https://en.wikipedia.org/wiki/Pell%27s_equation#Fundamental_solution_via_continued_fractions
  [D]
  (let [cf (sqrt-continued-fraction D)
        period (count (:cycle cf))
        convergent (continued-fraction-convergent cf (if (= (mod period 2)
                                                            0)
                                                          period
                                                          (* 2 period)))]
    (if (= period 0)
      nil
      (if (integer? convergent)
        [convergent 1]
        [(numerator convergent) (denominator convergent)]))))

(defn problem66
  "
  Consider quadratic Diophantine equations of the form:
    x^2 - Dy^2 = 1
  For example, when D = 13, the minimal solution in x is 649^2 - 13 * 180^2 = 1.

  It can be assumed that there are no solutions in positive integers when D is square.

  By finding minimal solutions in x for D = {2,3,5,6,7}, we obtain the following:
    3^2 - 2 * 2^2 &= 1\\
    2^2 - 3 * 1^2 &= 1\\
    {color{red}{mathbf 9}}^2 - 5 * 4^2 &= 1\\
    5^2 - 6 * 2^2 &= 1\\
    8^2 - 7 * 3^2 &= 1
  Hence, by considering minimal solutions in x for D<=7, the largest is obtained when D=5.

  Find the value of D<=1000 in minimal solutions of x for which the largest value of x is obtained.
  "
  ;; see https://en.wikipedia.org/wiki/Pell%27s_equation#Fundamental_solution_via_continued_fractions
  ([] (problem66 1000))
  ([d_max]
   (->> (range 2 (inc d_max))
        (remove square-number?)
        (map (fn [D] [D (pell-equation-fundamental-solution D)]))
        (sort-by (comp second second))
        (last))))

(defn problem67
  "
  By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

  3
  7 4
  2 4 6
  8 5 9 3

  That is, 3 + 7 + 4 + 9 = 23.

  Find the maximum total from top to bottom in triangle.txt (right click and 'Save Link/Target As...'), a 15K text file containing a triangle with one-hundred rows.

  NOTE: This is a much more difficult version of Problem 18.
  It is not possible to try every route to solve this problem, as there are 2^99 altogether!
  If you could check one trillion (10^12) routes every second it would take over twenty billion years to check them all.
  There is an efficient algorithm to solve it. ;o)
  "
  []
  (let [input (slurp (io/resource "0067_triangle.txt"))]
    (max-triangle-path input)))
