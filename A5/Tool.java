/**  DO NOT MODIFY THIS CLASS IN ANY WAY.
 * 
 * An enumeration of all available tools. */
public enum Tool {
	PENCIL("pencil"),
	ERASER("eraser"),
	COLOR_PICKER("color-picker"),
	AIRBRUSH("airbrush"),
	LINE("line"),
	CIRCLE("circle");
    
    private String string; // The name to use for toString

    /** Constructor: an instance with toString result name. */
    Tool(String name){
        string= name;
        }

   /** Return the name. */
    @Override public String toString() {
        return string;
    }
}
