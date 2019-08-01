/* Assignment for my Object-Oriented Programming and Data Structures course. */

import java.io.*;
import javax.imageio.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.filechooser.*;
import java.awt.event.*;
import java.awt.*;
import javax.swing.border.*;
import java.awt.image.*;

/** The class for the main window of the program.
 * Most of the GUI components are set up here. */
class PaintGUI extends JFrame implements ActionListener, ChangeListener {

    private final int iconSize= 48; // Size of icons for buttons.

    private final int drawRegionWidth= 700; // Width of drawing region.
    private final int drawRegionHeight= 520; // Height of drawing region.


    private final int defImgWidth= 640; // Default image (the canvas) width.
    private final int defImgHeight= 480; // Default image (the canvas) height.
    private final Color defImgBckColor= Color.WHITE; //Default background color.

    private int lastImgWidth= defImgWidth; // Width of last blank image created.
    private int lastImgHeight= defImgHeight; // Height of last blank image
    										 // created.

    private DrawingCanvas canvas; // The drawing canvas.

    private JLabel sizeLabel;           // Label for dimensions of image
    									//(the canvas).
    private JLabel mousePositionLabel;  // Label for position of mouse.
    private JLabel toolSizeLabel;     // Label for size of tool.
    private JLabel unsavedLabel;  // Label to inform user of unsaved changes.
    private final String unsavedMsg = "SAVE"; // Default message if unsaved
    										  // changes.

    private final int defToolSize= 1; // Default tool size.

    private JToggleButton pencil; // Pencil button.
    private JToggleButton eraser; // Eraser button.
    private JToggleButton colorPicker; // Color picker button.
    private JToggleButton airbrush; // Airbrush button.
    private JToggleButton line; // Line button.
    private JToggleButton circle; // Circle button.
    private JButton foreColorButton;  // Foreground color button.
    private JButton backColorButton; // Background color button.

    private JSlider toolSizeSlider; // Slider for choosing tool size
    private final int sliderMin= 1; // Minimum value for slider
    private final int sliderMax= 50; // Maximum value for slider
    private final int sliderInit= defToolSize; // Initial value for slider

    final static String defTitle= "CS2110 Paint"; // Default window title.

    File lastUsedFile;  // Last used file. */
    final String defFileName= "untitled.png"; // Default file name to save to.
    boolean imageUnsaved= false; //Whether the image has unsaved changes or not.

    /** Constructor: the main window of the program. */
    public PaintGUI() {
        super(defTitle);
        setLayout(new BorderLayout());

        JMenuBar menuBar= setUpMenuBar(); // Set up menu bar

        // Canvas & scroller
        canvas= new DrawingCanvas(this, defImgWidth, defImgHeight,
        		defImgBckColor, defToolSize);
        JScrollPane scroller= new JScrollPane(canvas);
        scroller.setPreferredSize(new Dimension(drawRegionWidth,
        		drawRegionHeight));

        JToolBar toolBar= setUpToolBar(); // Set up tool bar.

        createSlider(); // Set up Tool size slider

        JPanel statusPanel= setUpStatusBar(); // Set up status bar

        // Add to window
        add(menuBar, BorderLayout.NORTH);
        add(toolBar, BorderLayout.EAST);
        add(scroller, BorderLayout.CENTER);
        add(toolSizeSlider, BorderLayout.WEST);
        add(statusPanel, BorderLayout.SOUTH);

        pack();
        setLocationRelativeTo(null);
        setVisible(true);
    }

    /** Create and return the status panel. */
    private JPanel setUpStatusBar() {
        JPanel statusPanel= new JPanel();
        statusPanel.setBorder(new BevelBorder(BevelBorder.LOWERED));
        statusPanel.setPreferredSize(new Dimension(getWidth(), 18));
        statusPanel.setLayout(new GridLayout(1, 4));

        mousePositionLabel= new JLabel("Position:");
        mousePositionLabel.setHorizontalAlignment(SwingConstants.LEFT);
        statusPanel.add(mousePositionLabel);

        toolSizeLabel= new JLabel("Tool Size: " + canvas.getToolSize());
        statusPanel.add(toolSizeLabel);

        sizeLabel= new JLabel();
        updateSizeLabel();
        statusPanel.add(sizeLabel);

        unsavedLabel= new JLabel("");
        unsavedLabel.setForeground(Color.RED);
        statusPanel.add(unsavedLabel);

        return statusPanel;
    }

   /** Create the Tool Size slider --field toolSizeSlider*/
    private void createSlider() {
        toolSizeSlider= new JSlider(JSlider.VERTICAL, sliderMin, sliderMax,
        		sliderInit);
        toolSizeSlider.addChangeListener(this);
        toolSizeSlider.setMinorTickSpacing(1);
        toolSizeSlider.setPaintTicks(true);
        toolSizeSlider.setSnapToTicks(true);
    }

    /** Update the color of the foreground color button. */
    public void updateForeColor() {
        ImageIcon icon= getIcon(canvas.getForeColor(), iconSize);
        foreColorButton.setIcon(icon);
    }

    /** Update the color of the background color button. */
    public void updateBackColor() {
        ImageIcon icon= getIcon(canvas.getBackColor(), iconSize);
        backColorButton.setIcon(icon);
    }

    /**  Call this method to indicate that the image has been saved. */
    private void setImageSaved() {
        imageUnsaved= false;
        unsavedLabel.setText("");
    }

    /** Call this method to indicate that the image has unsaved changes. */
    public void setImageUnsaved() {
        imageUnsaved= true;
        unsavedLabel.setText(unsavedMsg);
    }

    /** Update the label that displays the mouse position to (x, y) */
    public void setMousePosition(int x, int y) {
        // TODO: #1. Implement me!
    	mousePositionLabel.setText("Position: ("+x+","+y+")");
    }

    /** If e is NOT the toolSizeSlider, print an error message.
     * If it IS the toolSizeSlider, then store the value of the slider
     * in field toolSize of the canvas and also put the value in the
     * status bar. */
    public void stateChanged(ChangeEvent e) {
        Object s= e.getSource();
        if (s == toolSizeSlider) {
            // TODO: #2. Implement me!
        	canvas.setToolSize(toolSizeSlider.getValue());
        	toolSizeLabel.setText("Tool Size: " + canvas.getToolSize());
        }
        else {
            System.err.println("stateChanged: " + s);
        }
    }

    /** Update the image (the canvas) size (dimensions) label, sizeLabel */
    private void updateSizeLabel() {
        // TODO: GRIES 3 Implement me! This depends on the current width/height
        //               of the image on the canvas.
    	sizeLabel.setText("Image: "+canvas.getImg().getWidth() + " x " +
    			canvas.getImg().getHeight());
    }

    /** Called to process action new */
    private void newAction(ActionEvent e) {
        System.out.println("Action: New");

        NewImageDialog dialog= new NewImageDialog(this, true, lastImgWidth,
        		lastImgHeight);
        Dimension d= dialog.getDimension();
        System.out.println("Dimension given in dialog: "+d);

        if (d != null) {
            canvas.newBlankImage(d.width, d.height, defImgBckColor);
            updateSizeLabel();

            lastUsedFile= null;
            setTitle(defTitle);
            setImageSaved();
        }
    }

    /** Process click of menu item File -> Open to open a file chosen by the
     *  user.
     */
    private void openAction(ActionEvent e) {
        System.out.println("Action: Open");

        JFileChooser chooser= new JFileChooser(".");
        FileNameExtensionFilter filter=
        		new FileNameExtensionFilter("Image Files", "jpeg", "jpg", "gif",
        		"png", "bmp");
        chooser.setFileFilter(filter);
        int returnVal= chooser.showOpenDialog(this);
        File selectedFile= chooser.getSelectedFile();
        if (returnVal != JFileChooser.APPROVE_OPTION) return;

        System.out.println("You chose to open this file: " +
        		selectedFile.getName());
        BufferedImage img= null;
        try {
            img= ImageIO.read(selectedFile);
        } catch (IOException exc) {
            System.out.println(exc.getMessage());
            return;
        }

        lastUsedFile = selectedFile;
        setTitle(defTitle + " - " + lastUsedFile.getName());
        setImageSaved();

        canvas.newImage(img);
        updateSizeLabel();
    }

    /** Save the image to file f.
     * @throws IOException. */
    private void saveImg(File f) throws IOException {
        String fileName= f.getName();
        int dotPosition= fileName.lastIndexOf(".");
        String format= fileName.substring(dotPosition+1);
        System.out.println("Saving in: " + fileName);
        System.out.println("Format: " + format);

        ImageIO.write(canvas.getImg(),format,f);
    }

    /** Process click of menu item File -> Save to save the file. */
    private void saveAction(ActionEvent e) {
        System.out.println("Action: Save");

        if (lastUsedFile == null) {
            saveAsAction(e);
        } else {
            try {
                saveImg(lastUsedFile);
            }
            catch(IOException exc) {
                System.err.println(exc.getMessage());
            }
            setImageSaved();
        }
    }

    /** Process click of menu item File -> SaveAs. */
    private void saveAsAction(ActionEvent e) {
        System.out.println("Action: Save As");

        JFileChooser chooser= new JFileChooser();
        if (lastUsedFile != null)
            chooser.setSelectedFile(lastUsedFile);
        else {
            File currentDir= new File("");
            String currentDirPath = currentDir.getAbsolutePath();
            File defaultFile= new File(currentDirPath + "/" + defFileName);
            chooser.setSelectedFile(defaultFile);
        }

        FileNameExtensionFilter filter =
        		new FileNameExtensionFilter("Image Files", "jpeg", "jpg", "gif",
        		"png", "bmp");
        chooser.setFileFilter(filter);
        int returnVal = chooser.showSaveDialog(this);
        File selectedFile = chooser.getSelectedFile();
        if (returnVal != JFileChooser.APPROVE_OPTION) return;

        System.out.println("You chose to save to the file: " +
        		selectedFile.getName());
        try {
            saveImg(selectedFile);

            lastUsedFile = selectedFile;
            setTitle(defTitle + " - " + lastUsedFile.getName());
            setImageSaved();
        }
        catch(IOException exc) {
            System.err.println(exc.getMessage());
        }
    }

    /** Process click of menu item File -> Quit. */
    private void quitAction(ActionEvent e) {
        System.out.println("Action: Quit");
        // TODO: #4. Implement me! Quit the application.
        System.exit(0);
        System.err.println("Implement me!");
    }

    /** Process click of menu item Help. */
    private void helpAction(ActionEvent e) {
        System.out.println("Action: Help");

        JOptionPane.showMessageDialog(this,"help...","Help",
        		JOptionPane.PLAIN_MESSAGE);
    }

    /** Process click on menu item Help -> About. */
    private void aboutAction(ActionEvent e) {
        System.out.println("Action: About");

        JOptionPane.showMessageDialog(this,"about...","About",
        		JOptionPane.PLAIN_MESSAGE);
    }

    /** Create a single-color icon of an image of dimension size x size and
     * color, meant to be used as an icon for the foreground/background color
     * buttons.  */
    private static ImageIcon getIcon(Color c, int size) {
        // TODO: #5. Implement me!
    	BufferedImage img= new BufferedImage(size, size,
    			BufferedImage.TYPE_INT_ARGB);
    	Graphics2D g2d= (Graphics2D) img.getGraphics();
    	g2d.setColor(c);
        g2d.fillRect(0, 0, size, size);
        return new ImageIcon(img);
    }

    /** Process event e from the toolbar */
    @Override public void actionPerformed(ActionEvent e) {
        System.out.println("actionPerformed");

        Object s = e.getSource();

        if (s == pencil) {
            canvas.setActiveTool(Tool.PENCIL); return;
        }
        if (s == eraser) {
            canvas.setActiveTool(Tool.ERASER); return;
        }
       if (s == colorPicker) {
            canvas.setActiveTool(Tool.COLOR_PICKER); return;
        }
        if (s == airbrush) {
            canvas.setActiveTool(Tool.AIRBRUSH); return;
        }
        if (s == line) {
            canvas.setActiveTool(Tool.LINE); return;
        }
        if (s == circle) {
            canvas.setActiveTool(Tool.CIRCLE); return;
        }
        if (s == foreColorButton) {
            Color newColor= JColorChooser.showDialog(
                            this,"Foreground Color",canvas.getForeColor());
            // TODO: #6. Implement me!
            if(newColor != null) {
            	canvas.setForeColor(newColor);
            	updateForeColor();
            }
            return; // Don't delete this return
        }
        if (s == backColorButton) {
            Color newBackColor= JColorChooser.showDialog(
                            this,"Background Color",canvas.getBackColor());
            // TODO: #7. Implement me!
            if(newBackColor != null) {
            	canvas.setBackColor(newBackColor);
            	updateBackColor();
            }
            return; // Don't delete this return
        }
        System.err.println(s);
    }

    /** Set up and return the menu bar. */
    private JMenuBar setUpMenuBar() {
        // Menu bar
        JMenuBar menuBar= new JMenuBar();

        JMenu fileMenu= fixFileMenu();
        JMenu helpMenu= fixHelpMenu();

        // Add to menu bar
        menuBar.add(fileMenu);
        menuBar.add(helpMenu);

        return menuBar;
    }

    /** Set up and return the file menu. */
    public JMenu fixFileMenu() {
        JMenu fileMenu= new JMenu("File");
        fileMenu.setMnemonic(KeyEvent.VK_F);

        JMenuItem newItem= new JMenuItem("New");
        newItem.setMnemonic(KeyEvent.VK_N);
        newItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N,
        		ActionEvent.CTRL_MASK));
        newItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                newAction(e);
            }
        });

        JMenuItem openItem= new JMenuItem("Open");
        openItem.setMnemonic(KeyEvent.VK_O);
        openItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O,
        		ActionEvent.CTRL_MASK));
        openItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                openAction(e);
            }
        });

        JMenuItem saveItem= new JMenuItem("Save");
        saveItem.setMnemonic(KeyEvent.VK_S);
        saveItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
        		ActionEvent.CTRL_MASK));
        saveItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                saveAction(e);
            }
        });

        JMenuItem saveAsItem= new JMenuItem("Save As");
        saveAsItem.setMnemonic(KeyEvent.VK_A);
        saveAsItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
        		ActionEvent.CTRL_MASK | ActionEvent.SHIFT_MASK));
        saveAsItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                saveAsAction(e);
            }
        });

        JMenuItem quitItem= new JMenuItem("Quit");
        quitItem.setMnemonic(KeyEvent.VK_Q);
        quitItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Q,
        		ActionEvent.CTRL_MASK));
        quitItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                quitAction(e);
            }
        });

        fileMenu.add(newItem);
        fileMenu.add(openItem);
        fileMenu.add(saveItem);
        fileMenu.add(saveAsItem);
        fileMenu.add(new JSeparator());
        fileMenu.add(quitItem);

        return fileMenu;
    }

    /** Set up and return the help menu. */
    public JMenu fixHelpMenu() {
        JMenu helpMenu= new JMenu("Help");
        helpMenu.setMnemonic(KeyEvent.VK_H);
        JMenuItem helpItem= new JMenuItem("Help");
        helpItem.setMnemonic(KeyEvent.VK_H);
        helpItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_H,
        		ActionEvent.CTRL_MASK));
        helpItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                helpAction(e);
            }
        });
        JMenuItem aboutItem= new JMenuItem("About");
        aboutItem.setMnemonic(KeyEvent.VK_A);
        aboutItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A,
        		ActionEvent.CTRL_MASK));
        aboutItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                aboutAction(e);
            }
        });
        helpMenu.add(helpItem);
        helpMenu.add(new JSeparator());
        helpMenu.add(aboutItem);
        return helpMenu;
    }

    /** Create a new JToggleButton for t, using image file named t.toString() +
     *  ".png".
     * Set its tool tip text to t.toString(). Add this as an actionlistener to
     * the new button. Add the new button to tools. Return the new button. */
    private JToggleButton fixJToggleButton(Tool t, ButtonGroup tools) {
        JToggleButton jtb=  new JToggleButton(new ImageIcon(t + ".png"));
        jtb.setToolTipText(t.toString());
        jtb.addActionListener(this);
        tools.add(jtb);
        return jtb;
    }

    /** Set up and return the tool bar. */
    private JToolBar setUpToolBar() {
        // Toolbar
        ButtonGroup tools= new ButtonGroup();
        pencil= fixJToggleButton(Tool.PENCIL, tools);
        colorPicker= fixJToggleButton(Tool.COLOR_PICKER, tools);
        eraser= fixJToggleButton(Tool.ERASER, tools);
        airbrush= fixJToggleButton(Tool.AIRBRUSH, tools);
        line= fixJToggleButton(Tool.LINE, tools);
        circle= fixJToggleButton(Tool.CIRCLE, tools);

        // Foreground color chooser
        ImageIcon icon= getIcon(canvas.getForeColor(),iconSize);

        // TODO: #5. After you implement method getIcon, you should use icons
        // instead of text. So uncomment the line below and comment-out the line
        // after..
        foreColorButton= new JButton(icon);
        //foreColorButton= new JButton("F. Color");

        foreColorButton.setToolTipText("foreground color");
        foreColorButton.addActionListener(this);

        // Background color chooser
        ImageIcon backIcon= getIcon(canvas.getBackColor(),iconSize);

        // TODO: #5. After you implement method getIcon, you should use icons
        // instead of text. So uncomment the line below and comment-out the line
        // after
        backColorButton= new JButton(backIcon);
        //backColorButton= new JButton("B. Color");

        backColorButton.setToolTipText("background color");
        backColorButton.addActionListener(this);

        return makeToolBar();
    }

    /** Set up the tool bar and return it. */
    private JToolBar makeToolBar() {
        JToolBar toolBar= new JToolBar(JToolBar.VERTICAL);
        toolBar.setFloatable(false);
        toolBar.setRollover(true);
        toolBar.add(pencil);
        toolBar.add(colorPicker);
        toolBar.add(eraser);
        toolBar.add(airbrush);
        toolBar.add(line);
        toolBar.add(circle);
        toolBar.add(foreColorButton);
        toolBar.add(backColorButton);
        return toolBar;

    }

    /** Start the GUI. */
    public static void main(String[] args) {
        PaintGUI mainWindow = new PaintGUI();
        mainWindow.canvas.revalidate();

        mainWindow.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    }
}
