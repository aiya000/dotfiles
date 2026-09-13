---
name: record-android-app-demo
description: Record a demo of an Android app on the adb-connected device and turn it into a README-ready GIF plus an mp4. Use when the user asks to record, film or capture a demo of an app on their phone, or invokes `/record-android-app-demo`.
---

# record-android-app-demo

Drive the app with `adb` while `screenrecord` runs, then cut the result into an inline GIF and a
linked mp4.

## The screen belongs to the user

- **Never press HOME or RECENTS, and never switch apps, to arrange a nicer background.** Whatever is
  on the device is the background of the demo, and the user may have chosen it deliberately. Ask
  before changing it
- **Ask before `pm clear`.** It wipes whatever the user had stored in the app. A demo that starts
  from an empty state usually needs it, so ask first, and keep a screenshot of the current state
- Agree on what the demo should show before recording. Re-recording is cheap; re-deciding is not

## Environment

- The device is usually on **wireless adb**, and the address changes between sessions. `adb devices`;
  when it lists none, ask the user for the address from
  設定 → 開発者向けオプション → ワイヤレスデバッグ
- A foldable has **two displays**. `adb shell cmd display get-displays` prints each one's
  `uniqueId "local:<id>"`, and `screencap` needs that id:

    ```console
    $ adb shell screencap -d 4630947123231501204 -p > shot.png   # Z Fold8, inner screen
    ```

    The outer screen of the same device is `4630947004648141459`

- `adb shell wm size` gives the pixel size to record at, and `wm density` the dp scale
  (`px = dp * density / 160`), which is how tap coordinates are worked out from the layout
- Claude Code blocks a foreground `sleep`, and a hook rejects `grep` / `find` anywhere in a command.
  So **put every sleep inside the `adb shell` script** (it runs on the device), and filter `dumpsys`
  output locally with `rg` instead of piping it through the device's `grep`

## Recording

1. Start `screenrecord` as a **background** Bash task:

    ```console
    $ adb shell screenrecord --size 1248x1972 --bit-rate 8M --time-limit 60 /sdcard/part1.mp4
    ```

    - `--size` is not optional on a foldable. Without it the file comes out 2448 wide with the screen
      sitting in a corner of a mostly black canvas. Use what `wm size` reported
    - It takes 2-3 seconds before it is really capturing. Lead in with a sleep, and trim later

2. Drive the app from a second `adb shell`, with **all sleeps on the device side**:

    ```console
    $ adb shell 'sleep 3
    am start -n io.example.app/.MainActivity >/dev/null 2>&1
    sleep 2.5
    input tap 620 700
    sleep 2
    input text "two%swords"
    input keyevent 66
    sleep 2.5
    pkill -INT screenrecord
    sleep 2'
    ```

    - `input text` takes `%s` for a space, and ASCII only
    - `pkill -INT screenrecord` is what finalizes the mp4. A hard kill leaves it unplayable

3. Pull it and **look at it as a contact sheet before trusting it**:

    ```console
    $ adb pull /sdcard/part1.mp4 <scratchpad>/part1.mp4
    $ ffmpeg -y -v error -i part1.mp4 -vf "fps=1,scale=200:-1,tile=10x2" -frames:v 1 sheet.png
    ```

    Reading that sheet is the only way to know a take is usable. Check the tail especially -- the last
    beat of a demo is the one that goes missing.

## Two gotchas that cost a take each

### The IME eats `input text`

A Japanese keyboard in kana mode turns `hello world` into `ｇらセリエκ…`. Check what is set, switch it
for the recording, and **put it back afterwards**:

```console
$ adb shell settings get secure default_input_method
$ adb shell ime list -s
$ adb shell ime set com.samsung.android.honeyboard/.service.HoneyBoardService
  ... record ...
$ adb shell ime set com.google.android.inputmethod.latin/com.android.inputmethod.latin.LatinIME
```

Samsung's keyboard passes ASCII through. Gboard in Japanese 12-key mode does not.

### `screenrecord` stops when a dismissed app is relaunched

Observed on the Z Fold8: the instant an activity that was finished comes back to the front, the
recording ends, and everything after that is simply not in the file. So **"close it, open it again,
the note is still there" cannot be a single take.** Record two and join them:

- part 1: launch → use → close
- part 2: `adb shell am force-stop <pkg>` first (a cold start does not stop the recording), then launch

Both parts open and close on the same background, so the seam does not show.

## Editing

Trim each part, then concatenate:

```console
$ ffmpeg -y -v error -ss 5.5 -to 16.8 -i part1.mp4 -c:v libx264 -crf 20 -pix_fmt yuv420p -an -r 30 p1.mp4
$ ffmpeg -y -v error -ss 2.0 -to 8.5  -i part2.mp4 -c:v libx264 -crf 20 -pix_fmt yuv420p -an -r 30 p2.mp4
$ ffmpeg -y -v error -i p1.mp4 -i p2.mp4 -filter_complex "[0:v][1:v]concat=n=2:v=1:a=0[v]" -map "[v]" \
    -c:v libx264 -crf 20 -pix_fmt yuv420p -r 30 demo-full.mp4
```

Keep `demo-full.mp4` at full resolution in the scratchpad. Every later output is derived from it, and
the user will ask for a change.

Then the two outputs a README wants:

```console
$ ffmpeg -y -v error -i demo-full.mp4 -vf "scale=720:-2" -c:v libx264 -crf 23 -preset slow \
    -pix_fmt yuv420p -movflags +faststart docs/demo.mp4
$ ffmpeg -y -v error -i demo-full.mp4 \
    -vf "fps=12,scale=320:-1:flags=lanczos,palettegen=stats_mode=diff" palette.png
$ ffmpeg -y -v error -i demo-full.mp4 -i palette.png \
    -lavfi "fps=12,scale=320:-1:flags=lanczos[x];[x][1:v]paletteuse=dither=bayer:bayer_scale=3" docs/demo.gif
```

`fps=12` at 320px keeps a 20 second take around 0.5MB. Embed them the way the other repositories do --
the GIF inline, the video as a small link underneath:

```markdown
<img src="docs/demo.gif" alt="..." width="320">

<sub>[video](docs/demo.mp4)</sub>
```

GitHub does not reliably resolve a relative `src` on a `<video>` tag, which is why the GIF is the one
that goes inline.

## Hiding part of the status bar

Notification icons date a recording and show what else is on the phone. Measure them from a frame,
then let `delogo` fill the area from its surroundings:

```console
$ ffmpeg -y -v error -ss 1 -i demo-full.mp4 -vframes 1 -vf "crop=1248:110:0:0" statusbar.png
  ... read statusbar.png, note the x/y/w/h of what has to go ...
$ ffmpeg -y -v error -i demo-full.mp4 -vf "delogo=x=152:y=28:w=192:h=66" \
    -c:v libx264 -crf 20 -pix_fmt yuv420p -r 30 demo-masked.mp4
```

**Do not paint a black `drawbox` over it.** A status bar is not `#000000`, and its shade changes from
scene to scene (`20,20,20` in one, `12,12,12` in another), so a black rectangle sits visibly on top of
it. `delogo` follows whatever is around it. Check the result at three different moments, not one.

Sampling a pixel, where the exact colour matters:

```console
$ ffmpeg -v error -ss 8 -i demo-full.mp4 -vframes 1 -vf "crop=w=2:h=2:x=600:y=60" \
    -f rawvideo -pix_fmt rgb24 - | od -An -tu1
```

`crop=1:1:x:y` fails to parse; give it `w=` / `h=` / `x=` / `y=`, and a size of at least 2.

## When it is done

- Delete the recordings left on the device (`/sdcard/part1.mp4`, `/sdcard/part2.mp4`) and any
  `/data/local/tmp` scratch files -- use `rm-dust` per the `fd` skill where it is available
- Put the IME back to what `settings get secure default_input_method` reported at the start
- Send the GIF with `SendUserFile` and let the user look at it before it is committed
