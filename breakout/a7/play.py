# play.py
# Emily Chen ec745
# 12/8/2016

"""Subcontroller module for Breakout

This module contains the subcontroller to manage a single game in the Breakout
App. Instances of Play represent a single game.  If you want to restart a new game, you
are expected to make a new instance of Play.

The subcontroller Play manages the paddle, ball, and bricks.  These are model objects.  
Their classes are defined in models.py.

Most of your work on this assignment will be in either this module or models.py.
Whether a helper method belongs in this module or models.py is often a complicated
issue.  If you do not know, ask on Piazza and we will answer."""
from constants import *
from game2d import *
from models import *


# PRIMARY RULE: Play can only access attributes in models.py via getters/setters
# Play is NOT allowed to access anything in breakout.py (Subcontrollers are not
# permitted to access anything in their parent. To see why, take CS 3152)

class Play(object):
    """An instance controls a single game of breakout.
    
    This subcontroller has a reference to the ball, paddle, and bricks. It animates the 
    ball, removing any bricks as necessary.  When the game is won, it stops animating.  
    You should create a NEW instance of Play (in Breakout) if you want to make a new game.
    
    If you want to pause the game, tell this controller to draw, but do not update.  See 
    subcontrollers.py from Lecture 25 for an example.
    
    INSTANCE ATTRIBUTES:
        _paddle [Paddle]: the paddle to play with 
        _bricks [list of Brick]: the list of bricks still remaining 
        _ball   [Ball, or None if waiting for a serve]:  the ball to animate
        _tries  [int >= 0]: the number of tries left 
    
    As you can see, all of these attributes are hidden.  You may find that you want to
    access an attribute in class Breakout. It is okay if you do, but you MAY NOT ACCESS 
    THE ATTRIBUTES DIRECTLY. You must use a getter and/or setter for any attribute that 
    you need to access in Breakout.  Only add the getters and setters that you need for 
    Breakout.
    
    You may change any of the attributes above as you see fit. For example, you may want
    to add new objects on the screen (e.g power-ups).  If you make changes, please list
    the changes with the invariants.
                  
    LIST MORE ATTRIBUTES (AND THEIR INVARIANTS) HERE IF NECESSARY
        
        _break_sound        [Instance of Sound]:
                            sound that plays when the ball collides with a brick
                        
        _bounce_sound       [Instance of Sound]:
                            sound that plays when the ball collides with the paddle
                        
        _labelTime          [int or float]:
                            the amount of time in which _collision_label is displayed
                        
        _collision_label    [GLabel, or None if there is no message to display]
                            the label that displays the number of bricks that the user
                            has destroyed
    """
    
    
    # GETTERS AND SETTERS (ONLY ADD IF YOU NEED THEM)
    def getBricks(self):
        '''Returns: a list of the game's bricks
        '''
        return self._bricks
    
    def getBall(self):
        '''Returns: the game's ball
        '''
        return self._ball
    
    def getPaddle(self):
        '''Returns: the game's paddle
        '''
        return self._paddle
    
    # INITIALIZER (standard form) TO CREATE PADDLES AND BRICKS
    def __init__(self):
        '''Initializer for Play class
        
           Creates the all the bricks for the game and creates the paddle
           for the game
        '''
        self.createBricks(BRICK_SEP_H/2 + BRICK_WIDTH/2, GAME_HEIGHT - BRICK_Y_OFFSET)
        self.createPaddle()
        self._ball = None
        self._break_sound = Sound('plate1.wav') #ADD SPECIFICATION
        self._bounce_sound = Sound('bounce.wav') #ADD SEPCIFICATION
        self._labelTime = 0 #ADD SPECIFICATION
        self._collision_label = None


    # UPDATE METHODS TO MOVE PADDLE, SERVE AND MOVE THE BALL
    def updatePaddle(self, inp):
        '''Moves the paddle left and right
            
           Checks whether the user is pressing left or right
           and moves the paddle respective to the direction
           
           Parameter inp: the user's key press
           Precondition: inp is an instance of GInput
        '''
        assert isinstance(inp, GInput)
        
        x = self._paddle.getX()
        if inp.is_key_down('left'):
            x -= PADDLE_SPEED
            if x < PADDLE_WIDTH/2:
                x = PADDLE_WIDTH/2
        if inp.is_key_down('right'):
            x += PADDLE_SPEED
            if x > (GAME_WIDTH - PADDLE_WIDTH/2):
                x = (GAME_WIDTH - PADDLE_WIDTH/2)
        self._paddle.setX(x)
 
    
    def updateBall(self,dt):
        '''Moves the ball

            Calls helper functions checkBrickCollision and checkPaddleCollision to detect collisions.
            
            If collision with bricks or paddle is not found, the method checks
            if ball has collided with the game window's sides or ceiling.
            If collision is found with the game window's sides or ceiling, the method
            changes the direction of the ball
            
           Parameter dt: The time in seconds since last update
           Precondition: dt is a number (int or float)
        '''
        assert type(dt) == float or type(dt) == int
        
        if self._labelTime > 0:
            self._labelTime -= dt
        if self._labelTime <= 0:
            self._collision_label = None
        vx = self._ball.getVx()
        vy = self._ball.getVy()
        self._ball.x += vx
        self._ball.y += vy
        collision_label = self.checkBrickCollision()
        if collision_label == None:
            if self.checkPaddleCollision() == False:
                if self._ball.x <= BALL_DIAMETER/2:
                    self._ball.setVx(-vx)
                    self._ball.x = BALL_DIAMETER/2
                if self._ball.x >= (GAME_WIDTH - BALL_DIAMETER/2):
                    self._ball.setVx(-vx)
                    self._ball.x = GAME_WIDTH - BALL_DIAMETER/2
                if self._ball.y > (GAME_HEIGHT - BALL_DIAMETER/2):
                    self._ball.setVy(-vy)
                if self._ball.y < 0:
                    self._ball = None
        else:
            self._collision_label = collision_label
            self._labelTime = 1
            

    # DRAW METHOD TO DRAW THE PADDLES, BALL, AND BRICKS
    def draw(self, view):
        '''Draws the game objects in the view
        
           Parameter view: the game view, used in drawing
           Precondition: view is an instance of GView
        '''
        assert isinstance(view, GView)
        
        for b in self._bricks:
            b.draw(view)
        if self._collision_label is not None:
            self._collision_label.draw(view)
        self._paddle.draw(view)
        if self._ball is not None:
            self._ball.draw(view)

    
    # HELPER METHODS FOR PHYSICS AND COLLISION DETECTION
    def checkBrickCollision(self):
        '''Returns: True if a collision is detected betwen the ball and a brick,
           False otherwise
        
           Checks whether or not the ball collided with a brick
           and changes the ball's direction if a collision is found.
           
           If a collision is found, the brick is removed from the list of
           bricks and a message displaying how many bricks the user has
           destroyed is displayed in its place
        
           Calls helper function bCollides(self,ball)
        '''
        vx = self._ball.getVx()
        vy = self._ball.getVy()
        for b in self._bricks if self._ball.getVy() < 0 else reversed(self._bricks):
            if b.bCollides(self._ball):
                if b.getXdir():
                    self._ball.setVx(-vx)
                if b.getYdir():
                    self._ball.setVy(-vy)
                self._bricks.remove(b)
                self._break_sound.play()
                return GLabel(text= str(BRICKS_IN_ROW * BRICK_ROWS - len(self._bricks)),font_size=12, linecolor=colormodel.BLACK, x=b.x, y=b.y)
        return None
    
    
    def checkPaddleCollision(self):
        '''Returns: True if a collision between the paddle and baln l is detected,
           False otherwise
           
           Checks whether or not the ball collided with the paddle
           and changes the ball's direction if a collision is found
           
           Calls helper function pCollides(self,ball)
        '''
        vx = self._ball.getVx()
        vy = self._ball.getVy()
        if self._paddle.pCollides(self._ball):
            if self._paddle.getXdir():
                self._ball.setVx(-vx)
            if self._paddle.getYdir():
                self._ball.setVy(-vy)
            self._bounce_sound.play()
            return True
        return False
    
    
    # ADD ANY ADDITIONAL METHODS (FULLY SPECIFIED) HERE
    def createBricks(self, x_pos, y_pos):
        '''Creates individual bricks and appends them to a list of
           bricks.
           
           Creates BRICKS_IN_ROW * BRICK_ROWS bricks
            
           Parameter x_pos: the x coordinate of the left-most brick
           Precondition: x_pos is a float or an int
           
           Parameter y_pos: the y coordinate of the highest brick
           Precondition: y_pos is a floaor an int
           
        '''
        assert type(x_pos) == float or type(x_pos) == int
        assert type(y_pos) == float or type(y_pos) == int #negative??
        
        self._bricks = []
        x_org = x_pos
        for i in range(BRICK_ROWS):
            for j in range(BRICKS_IN_ROW):
                color = BRICK_COLORS[(i/2) % 5]
                brick=Brick(x_pos, y_pos, color)
                self._bricks.append(brick)    
                x_pos = x_pos + BRICK_SEP_H + BRICK_WIDTH          
            y_pos = y_pos - BRICK_SEP_V - BRICK_HEIGHT
            x_pos = x_org
            
    def createPaddle(self):
        '''Creates paddle object
        '''
        self._paddle = Paddle()
        
    def serveBall(self):
        '''Creates ball object
        '''
        self._ball = Ball()