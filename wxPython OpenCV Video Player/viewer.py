import wx

import wx.lib.agw.buttonpanel as bp

import cv2

import numpy as np

import os.path

import time

import sys

import multiprocessing

# Demonstrates how to create a video player using OpenCV and wxPython
#
#
# Copyright (C) 2013  lassytouton
#
# This application is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 3 of the License, or (at your option) any later version.
#
# This application is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this application; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#
#
# Works with OpenCV 2.4.5
#
# Borrows from "Playing a movie" (http://opencv.willowgarage.com/wiki/wxpython)
#
# Icon camera.ico was sourced from
# Mart's (Marco Martin) Glaze icon set at http://www.iconfinder.com/search?q=iconset%3Aglaze
#
# Icons video.png, play.png, stop.png, and save.png were sourced from
# Ed Merritt's Vaga icon set at http://www.iconfinder.com/search/?q=iconset%3Avaga

if hasattr(sys, "frozen"):
    pathExe = sys.executable
else:
    pathExe = sys.argv[0]

if os.environ["PATH"][-1] != os.pathsep:
    os.environ["PATH"] += os.pathsep

os.environ["PATH"] += os.path.join(os.path.dirname(pathExe), "libraries/opencv/build/x86/mingw/bin")

class Video(object):
    def __init__(self, videoFile, frameNumber = 0):
        self._file = videoFile

        self._capture = cv2.VideoCapture(self._file)

        self._width = int(round(self._capture.get(cv2.cv.CV_CAP_PROP_FRAME_WIDTH)))
        self._height = int(round(self._capture.get(cv2.cv.CV_CAP_PROP_FRAME_HEIGHT)))

        self._totalFrames = int(round(self._capture.get(cv2.cv.CV_CAP_PROP_FRAME_COUNT)))

        self._framesPerSecond = self._capture.get(cv2.cv.CV_CAP_PROP_FPS)

        if ((frameNumber > 0) and (frameNumber < self._totalFrames)):
            self._capture.set(cv2.cv.CV_CAP_PROP_POS_FRAMES, frameNumber)

        self._frameNumber = int(round(self._capture.get(cv2.cv.CV_CAP_PROP_POS_FRAMES)))

        self._time = self._capture.get(cv2.cv.CV_CAP_PROP_POS_MSEC)

    def read(self, frameNumber = None):
        if (frameNumber is not None):
            if ((frameNumber > -1) and (frameNumber < self._totalFrames)):
                self._capture.set(cv2.cv.CV_CAP_PROP_POS_FRAMES, frameNumber)
            else:
                return (False, np.zeros((self._height, self._width, 3)))

        self._frameNumber = int(round(self._capture.get(cv2.cv.CV_CAP_PROP_POS_FRAMES)))

        self._time = self._capture.get(cv2.cv.CV_CAP_PROP_POS_MSEC) / 1000.0

        status, img = self._capture.read()

        return (status, img)

    @property
    def File(self):
        return self._file

    @property
    def Width(self):
        return self._width

    @property
    def Height(self):
        return self._height

    @property
    def TotalFrames(self):
        return self._totalFrames

    @property
    def FramesPerSecond(self):
        return self._framesPerSecond

    @property
    def FrameNumber(self):
        return self._frameNumber

    @property
    def Time(self):
        return self._time

class VideoFrame(wx.Frame):
    DEFAULT_TOTAL_FRAMES = 300

    DEFAULT_FRAME_WIDTH = 960
    DEFAULT_FRAME_HEIGHT = 540

    ID_OPEN = wx.NewId()
    ID_SLIDER = wx.NewId()
    ID_LABEL_FRAME_NUMBER = wx.NewId()
    ID_PLAY = wx.NewId()
    ID_STOP = wx.NewId()
    ID_SAVE_IMAGE = wx.NewId()

    MAX_PIXEL_VALUE = 255

    BUTTON_ENABLE_STATES = {True: "Normal", False: "Disabled"}

    def __init__(self, parent):
        wx.Frame.__init__(self, parent, -1, title = "viewer - version 1.1.0", style = wx.DEFAULT_FRAME_STYLE)

        os.environ["PATH"] = "%s;%s" % (os.path.abspath("./"), os.environ["PATH"]) 

        iconBundleName = os.path.join(os.path.dirname(pathExe), "icons/Glaze/camera.ico")
        if (os.path.exists(iconBundleName)):
            iconBundle = wx.IconBundleFromFile(iconBundleName, wx.BITMAP_TYPE_ANY)

            icon = iconBundle.GetIcon((32, 32))

            if (icon):
                self.SetIcon(icon)

        self.SetDoubleBuffered(True)

        self.video = None

        self.imgVideo = None

        self.imgWorking = None

        self.bmpVideo = None

        self.playing = False

        menuBar = wx.MenuBar()
        menu = wx.Menu()

        mnuOpenVideo = menu.Append(wx.ID_OPEN, "&Open\tAlt-O", "Open Video")

        self.Bind(wx.EVT_TOOL, self.onOpenVideo, mnuOpenVideo)

        menu.AppendSeparator()

        mnuPlay = menu.Append(self.ID_PLAY, "&Play\tAlt-P", "Play")

        self.Bind(wx.EVT_MENU, self.onPlay, mnuPlay)

        mnuStop = menu.Append(self.ID_STOP, "&Stop\tAlt-S", "Stop")

        self.Bind(wx.EVT_MENU, self.onStop, mnuStop)

        menu.AppendSeparator()

        mnuSaveImage = menu.Append(self.ID_SAVE_IMAGE, "Save &Image\tAlt-I", "Save Image")

        self.Bind(wx.EVT_MENU, self.onSaveImage, mnuSaveImage)

        menu.AppendSeparator()

        mnuExit = menu.Append(wx.ID_EXIT, "E&xit\tAlt-X", "Close window and exit program")

        self.Bind(wx.EVT_MENU, self.onClose, mnuExit)

        menuBar.Append(menu, "&File")

        menu = wx.Menu()

        mnuAbout = menu.Append(wx.ID_ABOUT, "&About\tAlt-A", "Information about this program")
        self.Bind(wx.EVT_MENU, self.onAbout, mnuAbout)

        menuBar.Append(menu, "&Help")

        self.SetMenuBar(menuBar)

        panel = wx.Panel(self, -1)

        sizer = wx.BoxSizer(wx.VERTICAL)

        panel.SetSizer(sizer)

        toolbar = bp.ButtonPanel(panel, -1)

        openVideo = bp.ButtonInfo(toolbar, self.ID_OPEN, wx.Bitmap(os.path.join(os.path.dirname(pathExe), "icons/Vaga/video.png"), wx.BITMAP_TYPE_PNG), shortHelp = "Open Video")
        toolbar.AddButton(openVideo)

        toolbar.AddSeparator()

        slider = wx.Slider(toolbar, self.ID_SLIDER, 0, 0, self.DEFAULT_TOTAL_FRAMES - 1, None, (self.DEFAULT_FRAME_WIDTH - 220, -1), wx.SL_HORIZONTAL)
        slider.Enable(True)
        slider.SetToolTip(wx.ToolTip("Video Slider"))
        toolbar.AddControl(slider, 1)

        toolbar.AddSeparator()

        lblFrameNumber = wx.TextCtrl(toolbar, self.ID_LABEL_FRAME_NUMBER, value="", size=(50, -1), style = wx.BORDER_NONE)
        lblFrameNumber.SetToolTip(wx.ToolTip("Video Frame Number"))
        lblFrameNumber.SetBackgroundColour(wx.SystemSettings.GetColour(wx.SYS_COLOUR_BTNFACE))
        lblFrameNumber.SetLabel("")
        lblFrameNumber.Enable(False)
        toolbar.AddControl(lblFrameNumber)

        toolbar.AddSeparator()

        play = bp.ButtonInfo(toolbar, self.ID_PLAY, wx.Bitmap(os.path.join(os.path.dirname(pathExe), "icons/Vaga/play.png"), wx.BITMAP_TYPE_PNG), shortHelp = "Play")
        toolbar.AddButton(play)

        stop = bp.ButtonInfo(toolbar, self.ID_STOP, wx.Bitmap(os.path.join(os.path.dirname(pathExe), "icons/Vaga/stop.png"), wx.BITMAP_TYPE_PNG), shortHelp = "Stop")
        toolbar.AddButton(stop)

        toolbar.AddSeparator()

        saveImage = bp.ButtonInfo(toolbar, self.ID_SAVE_IMAGE, wx.Bitmap(os.path.join(os.path.dirname(pathExe), "icons/Vaga/save.png"), wx.BITMAP_TYPE_PNG), shortHelp = "Save Image")
        toolbar.AddButton(saveImage)

        videoPanel = wx.Panel(panel, -1)

        sizer.Add(videoPanel, 1, wx.EXPAND)
        sizer.Add(toolbar, 0, wx.EXPAND)

        self.SetClientSize((self.DEFAULT_FRAME_WIDTH, 36 + self.DEFAULT_FRAME_HEIGHT))

        toolbar.DoLayout()

        panel.Layout()

        self.videoPanel = videoPanel

        self.mnuOpenVideo = mnuOpenVideo
        self.mnuPlay = mnuPlay
        self.mnuStop = mnuStop
        self.mnuSaveImage = mnuSaveImage
        self.openVideo = openVideo
        self.slider = slider
        self.lblFrameNumber = lblFrameNumber
        self.play = play
        self.stop = stop
        self.saveImage = saveImage

        videoPanel.Bind(wx.EVT_SIZE, self.onResizeVideo)

        self.Bind(wx.EVT_CLOSE, self.onClose)

        self.Bind(wx.EVT_BUTTON, self.onOpenVideo, openVideo)
        self.Bind(wx.EVT_SLIDER, self.onSlider, self.slider)
        self.Bind(wx.EVT_BUTTON, self.onPlay, play)
        self.Bind(wx.EVT_BUTTON, self.onStop, stop)
        self.Bind(wx.EVT_BUTTON, self.onSaveImage, saveImage)

        self.playTimer = wx.Timer(self)

        self.Bind(wx.EVT_TIMER, self.onNextFrame, self.playTimer)

        self.Bind(wx.EVT_UPDATE_UI, self.onUpdateUI)
        self.Bind(wx.EVT_PAINT, self.onPaint)

        self.Bind(wx.EVT_SIZE, self.onResize)

        if len(sys.argv) > 1:
            if os.path.exists(sys.argv[1]):
                wx.CallLater(0, self.openFile( os.path.abspath(sys.argv[1])))

        self.Show(True)

    def updateVideo(self, status, frame):
        if status:
            self.imgVideo = np.copy(frame)

            self.lblFrameNumber.SetLabel("%d" % self.video.FrameNumber)

            height, width = self.imgVideo.shape[:2]

            displayWidth, displayHeight = self.videoPanel.GetSize()

            aspectRatio = height * 1.0 / width

            areaWidth = displayWidth * int(round(displayWidth * aspectRatio))
            areaHeight = displayHeight * int(round(displayHeight / aspectRatio))

            if (areaWidth >= areaHeight):
                if (int(round(displayWidth * aspectRatio)) <= displayHeight):
                    resizeWidth = displayWidth
                    resizeHeight = int(round(displayWidth * aspectRatio))
                else:
                    resizeHeight = displayHeight
                    resizeWidth = int(round(displayHeight / aspectRatio))
            else:
                if (int(round(displayHeight / aspectRatio)) <= displayWidth):
                    resizeHeight = displayHeight
                    resizeWidth = int(round(displayHeight / aspectRatio))
                else:
                    resizeWidth = displayWidth
                    resizeHeight = int(round(displayWidth * aspectRatio))

            self.imgWorking = cv2.resize(self.imgVideo, (resizeWidth, resizeHeight))

            self.imgWorking = cv2.cvtColor(self.imgWorking, cv2.COLOR_BGR2RGB)

            height, width = self.imgWorking.shape[:2]

            self.bmpVideo = wx.BitmapFromBuffer(width, height, self.imgWorking.tostring())

            self.Refresh()

    def openFile(self, videoFile):
        self.videoFile = videoFile

        wx.GetApp().TopWindow.SetTitle("viewer - version 1.1.0  (%s)" % (os.path.basename(self.videoFile)))

        self.video = Video(self.videoFile)

        totalFrames = self.video.TotalFrames

        self.totalFrames = totalFrames

        self.slider.SetRange(0, self.totalFrames - 1)

        self.slider.SetValue(0)

        status, frame = self.video.read()

        self.updateVideo(status, frame)

    def updateFrame(self, frameNumber):
        if self.video:
            status, frame = self.video.read(frameNumber = frameNumber)

            self.updateVideo(status, frame)

    def onNextFrame(self, evt):
        if (self.video.FrameNumber < self.slider.GetMax()):
            if self.video:
                status, frame = self.video.read()

                self.updateVideo(status, frame)

                self.slider.SetValue(self.video.FrameNumber)
        else:
            self.onStop(None)

    def onOpenVideo(self, evt):
        filters = "AVI files (*.avi)|*.avi|All files (*.*)|*.*"

        dialog = wx.FileDialog(None, message = "Select video file...", defaultDir = "", wildcard = filters, style = wx.OPEN)

        if dialog.ShowModal() == wx.ID_OK:
            self.openFile(dialog.GetPath())

    def onSlider(self, evt):
        sliderValue = evt.EventObject.GetValue()
        if (self.video.FrameNumber != sliderValue):
            self.updateFrame(sliderValue)

    def onStop(self, evt):
        self.playTimer.Stop()

        self.playing = False

    def onPlay(self, evt):
        fps = self.video.FramesPerSecond

        if fps !=0:
            self.playTimer.Start(1000 / fps)#every X ms
        else:
            self.playTimer.Start(1000 / 15) #assuming 15 fps

        self.playing = True

    def onSaveImage(self, evt):
        if self.video:
            dirname = os.path.dirname(self.video.File)

            basename = os.path.splitext(os.path.basename(self.video.File))[0] + "-"

            basename = basename[:-1]
    
            imageSave = os.path.join(dirname, "%s-%s.png" % (basename, time.strftime("%Y-%m-%d-%H-%M-%S")))
    
            if os.path.isfile(imageSave):
                dlg = wx.MessageDialog(self,
                                       "%s exists... do you wish to overwrite?" % (imageSave),
                                       "Confirm Overwrite",
                                       wx.OK | wx.CANCEL | wx.ICON_QUESTION)
                result = dlg.ShowModal()
                dlg.Destroy()
                if result != wx.ID_OK:
                    return
    
            cv2.imwrite(imageSave, self.imgVideo)

    def onUpdateUI(self, evt):
        if (self.video):
            if (self.mnuOpenVideo.IsEnabled() != (not self.playing)):
                self.mnuOpenVideo.Enable(not self.playing)

            if (self.mnuPlay.IsEnabled() != (not self.playing)):
                self.mnuPlay.Enable(not self.playing)
            if (self.mnuStop.IsEnabled() != self.playing):
                self.mnuStop.Enable(self.playing)

            if (self.mnuSaveImage.IsEnabled() != (not self.playing)):
                self.mnuSaveImage.Enable(not self.playing)

            if (self.openVideo.IsEnabled() != (not self.playing)):
                self.openVideo.SetStatus(self.BUTTON_ENABLE_STATES[not self.playing])

            if (self.slider.IsEnabled() != (not self.playing)):
                self.slider.Enable(not self.playing)

            if (self.play.IsEnabled() != (not self.playing)):
                self.play.SetStatus(self.BUTTON_ENABLE_STATES[not self.playing])
            if (self.stop.IsEnabled() != self.playing):
                self.stop.SetStatus(self.BUTTON_ENABLE_STATES[self.playing])

            if (self.saveImage.IsEnabled() != (not self.playing)):
                self.saveImage.SetStatus(self.BUTTON_ENABLE_STATES[not self.playing])
        else:
            if (self.mnuOpenVideo.IsEnabled() != True):
                self.mnuOpenVideo.Enable(True)

            if (self.mnuPlay.IsEnabled() != False):
                self.mnuPlay.Enable(False)
            if (self.mnuStop.IsEnabled() != False):
                self.mnuStop.Enable(False)

            if (self.mnuSaveImage.IsEnabled() != False):
                self.mnuSaveImage.Enable(False)

            if (not self.mnuOpenVideo.IsEnabled()):
                self.mnuOpenVideo.SetStatus(self.BUTTON_ENABLE_STATES[True])

            if (not self.openVideo.IsEnabled()):
                self.openVideo.SetStatus(self.BUTTON_ENABLE_STATES[True])

            if (self.slider.IsEnabled()):
                self.slider.Enable(False)

            if (self.play.IsEnabled()):
                self.play.SetStatus(self.BUTTON_ENABLE_STATES[False])
            if (self.stop.IsEnabled()):
                self.stop.SetStatus(self.BUTTON_ENABLE_STATES[False])

            if (self.saveImage.IsEnabled()):
                self.saveImage.SetStatus(self.BUTTON_ENABLE_STATES[False])

        evt.Skip()

    def onPaint(self, evt):
        if self.bmpVideo:
            wx.BufferedPaintDC(self.videoPanel, self.bmpVideo)

        evt.Skip()

    def onResize(self, evt):
        clientWidth = self.GetClientSize()[0]

        self.slider.SetSize((clientWidth - 220, -1))

        evt.Skip()

    def onResizeVideo(self, evt):
        if (self.video):
            self.updateFrame(self.video.FrameNumber)

        evt.Skip()

    def onAbout(self, evt):
        wx.MessageBox("viewer uses OpenCV and wxPython to create a video player.\n\n\n"
                      "Copyright (C) 2013  lassytouton\n\n"
                      "This application is free software; you can redistribute it and/or\n"
                      "modify it under the terms of the GNU Lesser General Public\n"
                      "License as published by the Free Software Foundation; either\n"
                      "version 3 of the License, or (at your option) any later version.\n\n"
                      "This application is distributed in the hope that it will be useful,\n"
                      "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
                      "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\n"
                      "Lesser General Public License for more details.\n\n"
                      "You should have received a copy of the GNU Lesser General Public\n"
                      "License along with this application; if not, write to the Free Software\n"
                      "Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA\n\n\n"
                      "Works with OpenCV 2.4.5\n\n"
                      "Borrows from \"Playing a movie\" (http://opencv.willowgarage.com/wiki/wxpython)\n\n"
                      "Icon camera.ico was sourced from\n"
                      "Mart's (Marco Martin) Glaze icon set at http://www.iconfinder.com/search?q=iconset%3Aglaze\n\n"
                      "Icons video.png, play.png, stop.png, and save.png were sourced from\n"
                      "Ed Merritt's Vaga icon set at http://www.iconfinder.com/search/?q=iconset%3Avaga",
                      "viewer - version 1.1.0", wx.OK | wx.ICON_INFORMATION)

    def onClose(self, evt):
        self.onStop(None)

        self.Destroy()

if __name__=="__main__":
    multiprocessing.freeze_support()

    app = wx.App()
    app.RestoreStdio()
    VideoFrame(None)
    app.MainLoop()
