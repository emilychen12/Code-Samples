import javax.swing.*; 
import java.awt.event.*;
import java.awt.*;

/**  DO NOT MODIFY THIS CLASS IN ANY WAY.
 * 
 * An instance is a dialog window for new image. */
public class NewImageDialog extends JDialog implements ActionListener, FocusListener {
	private int defWidth, defHeight;
	private JButton createButton;
	private JButton cancelButton;
	private JTextField widthField;
	private JTextField heightField;
	private Dimension d;
	
	/** Return the dimensions of the image. */
    public Dimension getDimension() {
    	return d;
    }
    
    /** selectAll in event e if it is widthField or heightField.
     * Print an error if neither. */
    public void focusGained(FocusEvent e) {
    	Object s= e.getSource();
    	if (s == widthField) {
    		widthField.selectAll();
    	}
    	else if (s == heightField) {
    		heightField.selectAll();
    	}
    	else {
    		System.err.println("focusGained: " + s);
    	}
    }
    
    /** Check and fix the width or height, depending on whether e
     * is widthField or heightField. Print an error if neither. */
    public void focusLost(FocusEvent e) {
    	Object s= e.getSource();
    	if (s == widthField) {
    		checkAndFixWidth();
    	}
    	else if (s == heightField) {
    		checkAndFixHeight();
    	}
    	else {
    		System.err.println("focusLost: " + s);
    	}
    }
    
    /** Constructor: a new instance for frame in modal state, 
     * of size (w, h). */
    public NewImageDialog(JFrame frame, boolean modal, int w, int h) {
        super(frame,modal);
        setTitle("Create New Image");
        
        defWidth= w;
        defHeight= h;
        
        JPanel myPanel= new JPanel();
        getContentPane().add(myPanel);
        
        myPanel.add(new JLabel("Width"));
        widthField= new JTextField(""+w, 5);
        widthField.addFocusListener(this);
        myPanel.add(widthField);
        
        myPanel.add(new JLabel("Height"));
        heightField= new JTextField(""+h, 5);
        heightField.addFocusListener(this);
        myPanel.add(heightField);
        
        createButton= new JButton("Create");
        createButton.addActionListener(this);
        myPanel.add(createButton);
        
        cancelButton= new JButton("Cancel");
        cancelButton.addActionListener(this);
        myPanel.add(cancelButton);
        
        // set default button
        getRootPane().setDefaultButton(createButton);
        createButton.requestFocus();
        
        pack();
        setLocationRelativeTo(frame);
        setVisible(true);
    }

    /** Return the width given in field widthField.
     *  But if it is not an integer or is <= 0, set it to defWidth
     *  and return -1. */
    private int checkAndFixWidth() {
        int width = -1;
		try {
			width= Integer.parseInt(widthField.getText());
		}
		catch (NumberFormatException exc) {
			
		}
		if (width <= 0) {
			widthField.setText("" + defWidth);
			return -1;
		}
		
		return width;
    }
    
    /** Return the height given in field heightField.
     *  But if it is not an integer or is <= 0, set it to defHeight
     *  and return -1. */
    private int checkAndFixHeight() {
        int height= -1;
		try {
			height= Integer.parseInt(heightField.getText());
		}
		catch (NumberFormatException exc) {
			
		}
		if (height <= 0) {
			heightField.setText(""+defHeight);
			return -1;
		}
		
		return height;
    }
    
    /** Perform the action indicated by e --either the createButton or
     * the cancelButton. If neither, print and error message. */
    public void actionPerformed(ActionEvent e) {
    	System.out.println("actionPerformed");
    	Object s= e.getSource();
    	
        if (s == createButton) {
            System.out.println("User chose 'create'.");
            
            int width= checkAndFixWidth();
            if (width == -1) return;
            int height= checkAndFixHeight();
            if (height == -1) return;
            
            d= new Dimension(width, height);
            setVisible(false);
        }
        else if (s == cancelButton) {
            System.out.println("User chose 'cancel'.");
            d = null;
            setVisible(false);
        }
        else {
        	System.err.println(s);
        }
    }
    
}
