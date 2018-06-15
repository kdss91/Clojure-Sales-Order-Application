(require '[clojure.string :as str])

(def custMap {})
(def prodMap {})
(def salesMap {})

(defn readFiles[]
	(def custRead (str/split-lines (slurp "cust.txt")))
	(doseq [line custRead]
	(def custID (nth (str/split line #"\|") 0))
	(def cname (str "\"" (nth (str/split line #"\|") 1) "\""))
	(def address (str "\"" (nth (str/split line #"\|") 2) "\""))
	(def phone (str "\"" (nth (str/split line #"\|") 3) "\""))
	(def val1 (str cname " " address " " phone))
	(def custMap (merge-with into custMap {(Integer/parseInt custID)  val1})))
	
	(def prodRead (str/split-lines (slurp "prod.txt")))
	(doseq [line prodRead]
	(def prodID (nth (str/split line #"\|") 0))
	(def item (str "\"" (nth (str/split line #"\|") 1) "\""))
	(def cost (str "\"" (nth (str/split line #"\|") 2) "\""))
	(def val1 (str item " " cost ))
	(def prodMap (merge-with into prodMap {(Integer/parseInt prodID)  val1})))
	
	(def salesRead (str/split-lines (slurp "sales.txt")))
	(doseq [line salesRead]
	(def saleID (nth (str/split line #"\|") 0))
	(def custID (nth (str/split line #"\|") 1))
	(def prodID (nth (str/split line #"\|") 2))
	(def itemCount (nth (str/split line #"\|") 3))
	(def tmp (Integer/parseInt custID))
	(def tmp1 (Integer/parseInt prodID ))
	(def custName (str (nth (str/split (get custMap tmp) #"\"") 1) ))
	(def prodName (str (nth (str/split (get prodMap tmp1) #"\"") 1)))
	(def prodCost (str (nth (str/split (get prodMap tmp1) #"\"") 3)))
	(def val1 (str custID "|" custName "|" prodID "|" prodName "|" prodCost "|" itemCount))
	(def salesMap (merge-with into salesMap {(Integer/parseInt saleID)  val1}))))


(defn readCustomer[]	
	(doseq [item (sort-by first < custMap)]
	(print (str (nth item 0) ": "))
	(println (subvec item 1))))
	
(defn readProduct[]
	(doseq [item (sort-by first < prodMap)]
	(print (str (nth item 0) ": "))
	(println (subvec item 1))))

(defn readSales[]
	(doseq [item (sort-by first < salesMap)]
	(print (str (nth item 0) ": ["))	
	(print (str "\"" (nth (str/split (str (nth item 1)) #"\|") 1) "\" "))
	(print (str "\"" (nth (str/split (str (nth item 1)) #"\|") 3) "\" "))
	(println (str "\"" (nth (str/split (str (nth item 1)) #"\|") 5) "\"]"))))
	
(defn findSales[]
		(def flag "0")
		(println "Enter customer name:")
		(def cname (read-line))
		(def cID "")
		(def tmpName "")
		(doseq [item (sort-by first < custMap)]
		(cond 
			(= flag "0")(do (def tmpName (nth (str/split (str (nth item 1)) #"\"") 1))
							(cond 
								(= cname tmpName)(do (def cID (str (nth item 0))) (def flag "1"))
								:else ()))
			:else ()))
		(def costs [])
		(cond
			(= flag "1")(do (doseq [item salesMap] 
							(def prodCost (Float/parseFloat (nth (str/split (str (nth item 1)) #"\|") 4)))
							(def itemCount (Float/parseFloat (nth (str/split (str (nth item 1)) #"\|") 5)))
							(cond 
								(= cID (nth (str/split (str (nth item 1)) #"\|") 0)) (do (def costs (conj costs (* prodCost itemCount))))
								:else (do ())))
								(println)
								(print (str tmpName ": $")) 
								(println (format "%.2f" (double (reduce + costs)))))
			:else (do (println)(println "Customer does not exist."))))
			

(defn productCount[]
		(def flag "0")
		(println "Enter product name:")
		(def pname (read-line))
		(def pID "")
		(def tmpName "")
		(doseq [item (sort-by first < prodMap)]
		(cond 
			(= flag "0")(do (def tmpName (nth (str/split (str (nth item 1)) #"\"") 1))
							(cond 
								(= pname tmpName)(do (def pID (str (nth item 0))) (def flag "1"))
								:else ()))
			:else ()))
		(def counts1 [])
		(cond
			(= flag "1")(do (doseq [item salesMap] 
							(cond 
								(= pID (nth (str/split (str (nth item 1)) #"\|") 2)) (do (def counts1 (conj counts1 (Integer/parseInt (nth (str/split (str (nth item 1)) #"\|") 5)))))
								:else (do ())))
								(println)
								(print (str tmpName ": "))
								(println (reduce + counts1)))
			:else (do (println)(println "Product does not exist."))))
			

(defn Main []
	(def str1 "\n** Sales Menu **
------------------
1. Display Customer Table
2. Display Product Table
3. Display Sales Table
4. Total Sales for Customer
5. Total Count for Product
6. Exit
Enter an option?")
	(println str1)
	(while true
		(do		
			(def op (read-line))
			(println)
			(cond 
				(= op "1")(do(readCustomer)(println str1))
				(= op "2")(do(readProduct)(println str1))
				(= op "3")(do(readSales)(println str1))
				(= op "4")(do(findSales)(println str1))
				(= op "5")(do(productCount)(println str1))
				(= op "6")((println "Good Bye")(. System exit 0))
				:else (do (println "Enter a valid option.") (println str1))))))	

(readFiles)
(Main)
