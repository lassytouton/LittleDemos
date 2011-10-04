import wx
import cv2.cv as cv

# Updated for OpenCV 2.3.1
#
# Written by lassytouton
#
# Borrows from "Playing a movie" (http://opencv.willowgarage.com/wiki/wxpython)
#
# Icons film.png, control_stop.png, and control_play.png were sourced from
# Mark James' Silk icon set 1.3 at http://www.famfamfam.com/lab/icons/silk/

class CvVideoFrame(wx.Frame):
    DEFAULT_TOTAL_FRAMES = 300

    DEFAULT_FRAME_WIDTH = 300
    DEFAULT_FRAME_HEIGHT = 300

    ID_OPEN = 1
    ID_SLIDER = 2
    ID_STOP = 3
    ID_PLAY = 4

    ID_TIMER_PLAY = 5

    def __init__(self, parent):
        wx.Frame.__init__(self, parent, -1, title = "Video Player", style = wx.DEFAULT_FRAME_STYLE & ~wx.RESIZE_BORDER)

        self.SetDoubleBuffered(True)

        self.capture = None

        self.bmp = None

        self.playing = False

        self.displayPanel = wx.Panel(self, -1)

        self.ToolBar = self.CreateToolBar(style = wx.TB_BOTTOM | wx.TB_FLAT)

        openFile = self.ToolBar.AddLabelTool(self.ID_OPEN, '', wx.Bitmap('film.png'))
        self.ToolBar.AddSeparator()

        self.slider = wx.Slider(self.ToolBar, self.ID_SLIDER, 0, 0, self.DEFAULT_TOTAL_FRAMES - 1, None, (self.DEFAULT_FRAME_WIDTH - 100, 50), wx.SL_HORIZONTAL)
        self.ToolBar.AddControl(self.slider)
        self.ToolBar.AddSeparator()

        stop = self.ToolBar.AddLabelTool(self.ID_STOP, '', wx.Bitmap('control_stop.png'))
        play = self.ToolBar.AddLabelTool(self.ID_PLAY, '', wx.Bitmap('control_play.png'))

        self.Bind(wx.EVT_TOOL, self.onOpenFile, openFile)
        self.Bind(wx.EVT_SLIDER, self.onSlider, self.slider)
        self.Bind(wx.EVT_TOOL, self.onStop, stop)
        self.Bind(wx.EVT_TOOL, self.onPlay, play)

        self.playTimer = wx.Timer(self, self.ID_TIMER_PLAY)

        self.Bind(wx.EVT_TIMER, self.onNextFrame, self.playTimer)

        self.Bind(wx.EVT_IDLE, self.onIdle)
        self.Bind(wx.EVT_PAINT, self.onPaint)

        self.SetSize((self.DEFAULT_FRAME_WIDTH, self.DEFAULT_FRAME_HEIGHT))

        self.ToolBar.Realize()

        self.Show(True)

    def updateVideo(self):
        frame = cv.QueryFrame(self.capture)
        if frame:
            cv.CvtColor(frame, frame, cv.CV_BGR2RGB)

            self.bmp.CopyFromBuffer(frame.tostring())

            self.Refresh()

    def onNextFrame(self, evt):
        frameNumber = cv.GetCaptureProperty(self.capture, cv.CV_CAP_PROP_POS_FRAMES)

        self.slider.SetValue(frameNumber)

        self.updateVideo()

        evt.Skip()

    def onOpenFile(self, evt):
        filters = 'AVI files (*.avi)|*.avi|All files (*.*)|*.*'

        dialog = wx.FileDialog ( None, message = 'Select AVI files....', wildcard = filters, style = wx.OPEN | wx.MULTIPLE )

        if dialog.ShowModal() == wx.ID_OK:
            videoFile = dialog.GetPath()

            self.capture = cv.CaptureFromFile(videoFile)

            totalFrames = cv.GetCaptureProperty(self.capture, cv.CV_CAP_PROP_FRAME_COUNT)

            self.slider.SetRange(0, totalFrames)

            self.slider.SetValue(0)

            self.onSlider(wx.EVT_SLIDER)

            frame = cv.QueryFrame(self.capture)
            if frame:
                cv.CvtColor(frame, frame, cv.CV_BGR2RGB)

                self.bmp = wx.BitmapFromBuffer(frame.width, frame.height, frame.tostring())

                self.SetSize((frame.width, frame.height))

                sliderSize = self.slider.GetSize()

                self.slider.SetClientSizeWH(frame.width - 100, sliderSize.GetHeight())

                self.ToolBar.Realize()

    def onSlider(self, evt):
        frameNumber = cv.GetCaptureProperty(self.capture, cv.CV_CAP_PROP_POS_FRAMES)
        if (frameNumber != self.slider.GetValue()):
            cv.SetCaptureProperty(self.capture, cv.CV_CAP_PROP_POS_FRAMES, self.slider.GetValue())

            self.updateVideo()

    def onStop(self, evt):
        self.playTimer.Stop()

        self.playing = False

    def onPlay(self, evt):
        fps = cv.GetCaptureProperty(self.capture, cv.CV_CAP_PROP_FPS)
        if fps!=0:
            self.playTimer.Start(1000/fps)#every X ms
        else:
            self.playTimer.Start(1000/15)#assuming 15 fps

        self.playing = True

    def onIdle(self, evt):
        if (self.capture):
            if (self.ToolBar.GetToolEnabled(self.ID_OPEN) != (not self.playing)):
                self.ToolBar.EnableTool(self.ID_OPEN, not self.playing)
            if (self.slider.Enabled != (not self.playing)):
                self.slider.Enabled = not self.playing
            if (self.ToolBar.GetToolEnabled(self.ID_STOP) != self.playing):
                self.ToolBar.EnableTool(self.ID_STOP, self.playing)
            if (self.ToolBar.GetToolEnabled(self.ID_PLAY) != (not self.playing)):
                self.ToolBar.EnableTool(self.ID_PLAY, not self.playing)
        else:
            if (not self.ToolBar.GetToolEnabled(self.ID_OPEN)):
                self.ToolBar.EnableTool(self.ID_OPEN, True)
            if (self.slider.Enabled):
                self.slider.Enabled = False
            if (self.ToolBar.GetToolEnabled(self.ID_STOP)):
                self.ToolBar.EnableTool(self.ID_STOP, False)
            if (self.ToolBar.GetToolEnabled(self.ID_PLAY)):
                self.ToolBar.EnableTool(self.ID_PLAY, False)

    def onPaint(self, evt):
        if self.bmp:
            wx.BufferedPaintDC(self.displayPanel, self.bmp)

        evt.Skip()

if __name__=="__main__":
    app = wx.App()
    app.RestoreStdio()
    CvVideoFrame(None)
    app.MainLoop()
