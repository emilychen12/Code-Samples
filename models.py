# Assignment for my Introduction to Computing Using Python course
# 12/7/2016

"""Models module for Breakout

This module contains the model classes for the Breakout game. That is anything that you
interact with on the screen is model: the paddle, the ball, and any of the bricks.

Technically, just because something is a model does not mean there has to be a special
class for it.  Unless you need something special, both paddle and individual bricks could
just be instances of GRectangle.  However, we do need something special: collision 
detection.  That is why we have custom classes.

You are free to add new models to this module.  You may wish to do this when you add
new features to your game."""
import random # To randomly generate the ball velocity
from constants import *
from game2d import *


# PRIMARY RULE: Models are not allowed to access anything except the module constants.py.
# If you need extra information from Play, then it should be a parameter in your method,
# and Play should pass it as a argument when it calls the method.


class Paddle(GRectangle):
    """An instance is the game paddle.

    This class contains a method to detect collision with the ball, as well as move it
    left and right.  You may wish to add more features to this class.

    The attributes of this class are those inherited from GRectangle.

    LIST MORE ATTRIBUTES (AND THEIR INVARIANTS) HERE IF NECESSARY

    _xdir           [bool]:
                    is True if the ball's side has collided with the paddle's side,
                    False otherwise

    _ydir           [bool]:
                    is True if the ball's side has collided with the paddle's top
                    or bottom, False otherwise



    """
    # GETTERS AND SETTERS (ONLY ADD IF YOU NEED THEM)
    def getXdir(self):
        '''Returns: True if the ball's side has collided with the paddle's side,
        False otherwise
        '''
        return self._xdir


    def getYdir(self):
        '''Returns: True if the ball's top or bottom has collided wth the paddle's
        top or bottom, False otherwise
        '''
        return self._ydir


    def getX(self):
        '''Returns: the x coordinate of the paddle
        '''
        return self.x


    def setX(self, x):
        '''Sets the x coordinate of the paddle to a new x value

           Parameter x: the new x coordinate of the paddle
           Precondition: x is a float or an int
        '''

        assert type(x) == float or type(x) == int
        self.x = x


    # INITIALIZER TO CREATE A NEW PADDLE
    def __init__(self):
        '''Initializer for Paddle class

           Creates the game's paddle
        '''
        GRectangle.__init__(self, x = GAME_WIDTH/2 ,y=PADDLE_OFFSET,width=PADDLE_WIDTH,height=PADDLE_HEIGHT,fillcolor=colormodel.BLACK, linecolor=colormodel.BLACK)


    # METHODS TO MOVE THE PADDLE AND CHECK FOR COLLISIONS
    def pCollides(self, ball):
        '''Returns: True if the paddle's side has collided with the ball's side
        or if the paddle's top or bottom has collided with the ball's top or bottom,
        False otherwise

           Parameter ball: the game's ball
           Precondition: ball is an instance of Ball
        '''
        assert isinstance(ball, Ball)

        self._xdir = False
        self._ydir = False
        x = ball.getX()
        y = ball.getY()
        left_x = self.x - BALL_DIAMETER/2 - PADDLE_WIDTH/2
        right_x = self.x + BALL_DIAMETER/2 + PADDLE_WIDTH/2
        top_y = self.y + BALL_DIAMETER/2 + PADDLE_HEIGHT/2
        bot_y = self.y - BALL_DIAMETER/2 - PADDLE_HEIGHT/2
        if bot_y < y and y < top_y:
            if ball.getVx() > 0 and x >= left_x and (x - ball.getVx()) < left_x:
                x = left_x
                self._xdir = True
            if ball.getVx() < 0 and x <= right_x and (x - ball.getVx()) > right_x:
                x = right_x
                self._xdir = True
        if x > left_x and x < right_x:
            if ball.getVy() < 0 and y <= top_y and (y - ball.getVy()) > top_y:
                y = top_y
                self._ydir = True
        return self._xdir or self._ydir


class Brick(GRectangle):
    """An instance is the game brick.

    This class contains a method to detect collision with the ball.  You may wish to
    add more features to this class.

    The attributes of this class are those inherited from GRectangle.

    LIST MORE ATTRIBUTES (AND THEIR INVARIANTS) HERE IF NECESSARY

    _xdir           [bool]:
                    is True if the ball's side has collided with the brick's side,
                    False otherwise

    _ydir           [bool]:
                    is True if the ball's side has collided with the brick's top
                    or bottom, False otherwise
    """

    # GETTERS AND SETTERS (ONLY ADD IF YOU NEED THEM)
    def getXdir(self):
        '''Returns: True if the ball's side has collided with the brick's side,
        False otherwise
        '''
        return self._xdir


    def getYdir(self):
        '''Returns: True if the ball's top or bottom has collided with the brick's
        top or bottom, False otherwise
        '''
        return self._ydir


    # INITIALIZER TO CREATE A BRICK
    def __init__(self, x_pos, y_pos, color):
        '''Initializer for Brick Class

           Creates one brick

           Parameter x_pos: the x coordinate of the brick SPECIFY IT IS THE CENTER
           Precondition: x_pos is an int or float

           Parameter y_pos: the y coordinate of the brick
           Precondition: y_pos is an int or float

           Parameter color: the color of the brick
           Precondition: color is an instance of colormodel.RGB
        '''
        assert type(x_pos) == float or type(x_pos) == int
        assert type(y_pos) == float or type(y_pos) == int
        assert isinstance(color, colormodel.RGB)

        GRectangle.__init__(self,x=x_pos, y=y_pos, width=BRICK_WIDTH, height=BRICK_HEIGHT,fillcolor=color, linecolor=color)


    # METHOD TO CHECK FOR COLLISION
    def bCollides(self, ball):
        '''Returns: True if the brick's side has collided with the ball's side
        or if the brick's top or bottom has collided with the ball's top or bottom,
        False otherwise

           Parameter ball: the game's ball
           Precondition: ball is an instance of Ball
        '''
        assert isinstance(ball, Ball)

        self._xdir = False
        self._ydir = False
        x = ball.getX()
        y = ball.getY()
        left_x = self.x - BALL_DIAMETER/2 - BRICK_WIDTH/2
        right_x = self.x + BALL_DIAMETER/2 + BRICK_WIDTH/2
        top_y = self.y + BALL_DIAMETER/2 + BRICK_HEIGHT/2
        bot_y = self.y - BALL_DIAMETER/2 - BRICK_HEIGHT/2
        if bot_y < y and y < top_y:
            if ball.getVx() > 0 and x >= left_x and (x - ball.getVx()) < left_x:
                x = left_x
                self._xdir = True
            if ball.getVx() < 0 and x <= right_x and (x - ball.getVx()) > right_x:
                x = right_x
                self._xdir = True
        if x > left_x and x < right_x:
            if ball.getVy() < 0 and y <= top_y and (y - ball.getVy()) > top_y:
                y = top_y
                self._ydir = True
            if ball.getVy() > 0 and y >= bot_y and (y - ball.getVy()) < bot_y:
                y = bot_y
                self._ydir = True
        return self._xdir or self._ydir


class Ball(GImage):
    """Instance is a game ball.

    We extend GEllipse because a ball must have additional attributes for velocity.
    This class adds this attributes and manages them.

    INSTANCE ATTRIBUTES:
        _vx [int or float]: Velocity in x direction
        _vy [int or float]: Velocity in y direction

    The class Play will need to look at these attributes, so you will need
    getters for them.  However, it is possible to write this assignment with no
    setters for the velocities.

    How? The only time the ball can change velocities is if it hits an obstacle
    (paddle or brick) or if it hits a wall.  Why not just write methods for these
    instead of using setters?  This cuts down on the amount of code in Gameplay.

    NOTE: The ball does not have to be a GEllipse. It could be an instance
    of GImage (why?). This change is allowed, but you must modify the class
    header up above.

    LIST MORE ATTRIBUTES (AND THEIR INVARIANTS) HERE IF NECESSARY
    """

    # GETTERS AND SETTERS (ONLY ADD IF YOU NEED THEM)
    def getVx(self):
        '''Returns: the ball's horizontal velocity
        '''
        return self._vx


    def setVx(self, vx):
        '''Sets the ball's horizontal velocity to a new
        horizontal velocity

           Parameter vx: the ball's new horizontal velocity
           Precondition: vx is a float or int
        '''
        assert type(vx) == int or type(vx) == float
        self._vx = vx


    def getVy(self):
        '''Returns: the ball's vertical velocity
        '''
        return self._vy


    def setVy(self, vy):
        '''Sets the ball's vertical velocity to a new
        vertical velocity

           Parameter vy: the ball's new vertical velocity
           Precondition: vy is a float or int
        '''
        assert type(vy) == int or type(vy) == float
        self._vy = vy


    def getX(self):
        '''Returns: the ball's x coordinate
        '''
        return self.x


    def getY(self):
        '''Returns: the ball's y coordinate
        '''
        return self.y


    # INITIALIZER TO SET RANDOM VELOCITY
    def __init__(self):
        '''Initializer for Ball class

           Creates the game's ball and sets the ball's horizontal
           and vertical velocity
        '''
        GImage.__init__(self,x=GAME_WIDTH/2,y=GAME_HEIGHT/2,width=BALL_DIAMETER,height=BALL_DIAMETER,source='beach-ball.png')
        self._vy = -random.random()*(BALL_SPEED/2) - (BALL_SPEED/2.2)     #self._vx is never zero
        self._vx = (BALL_SPEED ** 2 - self._vy ** 2) ** (1/2.0)
