/* Assignment for my Object-Oriented Programming and Data Structures course. */

package student;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import models.Edge;
import models.Node;

/** This class contains a modified version of Dijkstra's shortest-path algorithm and some
 * other methods. */
public class Paths {

    /** Return the shortest path from start to end, or the empty list if a path
     * does not exist. The speed of the ship is dependent on whether the node has
     * a speed upgrade or is hostile. The speed can never go below 1.0. If the path
     * calculated has traverses three hostile planets (nodes), the path will be
     * recalculated if there are alternate paths.
     * Note: The empty list is NOT "null"; it is a list with 0 elements. */
    public static List<Node> shortestPath(Node start, Node end) {
        /* TODO Read note A7 FAQs on the course piazza for ALL details. */
        Heap<Node> F= new Heap<Node>(); // As in lecture slides
        // map contains an entry for each node in S or F. Thus,
        // |map| = |S| + |F|.
        // For each such key-node, the value part contains the shortest known
        // distance to the node and the node's backpointer on that shortest
        // path.
        HashMap<Node, SFdata> map= new HashMap<Node, SFdata>();
        F.add(start, 0);
        map.put(start, new SFdata(0, null, 1.0));
        // invariant: as in lecture slides, together with def of F and map
        while (F.size() != 0) {
            Node f= F.poll();
            if (f == end) return constructPath(end, map);
            double fTime= map.get(f).time;
            double fSpeed = map.get(f).speed;
            if(f.isHostile()) {
            	if(fSpeed >= 1.2) fSpeed -= 0.2;
            }
            if(f.hasSpeedUpgrade()) {
            	fSpeed += f.isHostile() ? 1.2 : 0.2;
            }
        	int hostileCount = getHostileCount(map,f);
            for (Edge e : f.getExits()) {// for each neighbor w of f
                Node w= e.getOther(f);
                boolean isAlive = w.isHostile() ? hostileCount < 2 : true;
                double newWTime= fTime + (e.length/fSpeed);
                if(isAlive) {
                    SFdata wData= map.get(w);
                    if (wData == null) { //if w not in S or F
                        map.put(w, new SFdata(newWTime,f,fSpeed));
                        F.add(w, newWTime);
                    } else if (newWTime < wData.time) {
                        wData.time= newWTime;
                        wData.speed = fSpeed;
                        wData.backPointer= f;
                        F.updatePriority(w, newWTime);
                    }
                }
            }
        }
        // no path from start to end
    	return new LinkedList<Node>();
    }

    /** Return the path from the start node to node end.
     *  Precondition: nData contains all the necessary information about
     *  the path. */
    public static List<Node> constructPath(Node end, HashMap<Node,SFdata> nData) {
        LinkedList<Node> path= new LinkedList<Node>();
        Node p= end;
        // invariant: All the nodes from p's successor to the end are in
        //            path, in reverse order.
        while (p != null) {
            path.addFirst(p);
            p= nData.get(p).backPointer;
        }
        return path;
    }


    /** Return the sum of the weights of the edges on path path. */
    public static int pathDistance(List<Node> path) {
        if (path.size() == 0) return 0;
        synchronized(path) {
            Iterator<Node> iter= path.iterator();
            Node p= iter.next();  // First node on path
            int s= 0;
            // invariant: s = sum of weights of edges from start to p
            while (iter.hasNext()) {
                Node q= iter.next();
                s= s + p.getConnect(q).length;
                p= q;
            }
            return s;
        }
    }

    /** Return the number of hostile planets encountered. **/
    private static int getHostileCount(HashMap<Node,SFdata> map, Node start) {
        Node p= start;
        int n = 0;
        while (p != null) {
        	if(p.isHostile()) {
        		n++;
        	}
            p= map.get(p).backPointer;
        }
        return n;
    }

    /** An instance contains information about a node: the previous node
     *  on a shortest path from the start node to this node, the amount of time
     *  the ship traveled from this node from the start node and the ship's
     *  speed. */
    private static class SFdata {
        private double time; // time from start node to this one
        private Node backPointer; // backpointer on path from start node to this one
        private double speed; // speed of the ship

        /** Constructor: an instance with time t from the start node,
         *  backpointer p and speed s.*/
        private SFdata(double t, Node p, double s) {
            time= t;     // Time from start node to this one.
            backPointer= p;  // Backpointer on the path (null if start node)
            speed = s;	// Speed of the ship
        }

        /** return a representation of this instance. */
        public String toString() {
            return "dist " + time + ", bckptr " + backPointer + ", speed " +
            	speed;
        }
    }
}
