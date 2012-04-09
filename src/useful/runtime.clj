(ns useful.runtime)

(def java-runtime (Runtime/getRuntime))

(defn free-memory-b
  "Returns the amount of free memory in the Java Virtual Machine."
  [] (.freeMemory java-runtime))

(defn total-memory-b
  "Returns the total amount of memory in the Java Virtual Machine."
  [] (.totalMemory java-runtime))

(defn max-memory-b
  "Returns the maximum amount of memory that the Java virtual machine will
  attempt to use.

  If there is no inherent limit then the value Long.MAX_VALUE will be
  returned."
  [] (.maxMemory java-runtime))

(defn used-memory-b
  "Returns the amount of memory used in the Java Virtual Machine."
  [] (- (total-memory-b) (free-memory-b)))
