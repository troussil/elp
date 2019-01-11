import java.util.*;

class MultiMap<K, V> extends HashMap<K, List<V> > implements Map<K, List<V> > {
    public void putOneValue(K aKey, V aValue) {
	List<V> storedLst = get(aKey);
	if (storedLst == null)
	    storedLst = new ArrayList<V>();
	storedLst.add(aValue); 
	put(aKey, storedLst); 
    }
    public boolean containsOneValue(V aValue) {
	for (List<V> storedLst : values()) {
	    if (storedLst.contains(aValue))
		return true; 
	}
	return false; 
    }
}
