---
title: AttendEase
meta: Android, Java · Mar to Apr 2026
links: <a href="https://appetize.io/app/b_wpwek676k67podfwefczyucynu">Try it in your browser</a> · <a href="https://github.com/ngzhankang/AttendEase/blob/main/docs/video/AttendEase-demo.mp4">Demo video</a> · <a href="https://github.com/ngzhankang/AttendEase/releases/latest">APK</a> · <a href="https://github.com/ngzhankang/AttendEase">Source</a>
---

An Android app where lecturers take attendance by tapping students' NFC
cards on their phone. Students see their attendance per module, and
lecturers run sessions and export the records as CSV. Everything is stored
on the device, so there is no backend or account setup.

<div class="project-media hero">
<figure>
<img src="/images/projects/attendease/nfc-scan.gif" width="240" height="514" loading="lazy" alt="A real NFC card tapped on an Android phone: Card Not Recognised, linked to a student, Attendance Registered, then Already Registered">
<figcaption>A real card on a Galaxy Note20 Ultra: unknown card, linked to a student, registered, then already registered.</figcaption>
</figure>
</div>

## What it does

- **Scanning.** When a lecturer starts a session, a full-screen scanner reads
  each card's ID and matches it to a student. The result is meant to be read
  from across a room: green for registered, amber for already registered,
  red for an unknown card.
- **Linking cards.** An unknown card can be linked on the spot to an enrolled
  student who has no card yet, which also registers their attendance.
- **Students** get a live banner while a session is open, a list of upcoming
  lessons, a month calendar, and attendance per module with a progress bar
  coloured by how much they have attended.
- **Lecturers** see today's schedule, start or stop attendance per lesson,
  add a student by ID, and export a module's attendance as CSV.

<div class="project-media">
<figure>
<img src="/images/projects/attendease/student_home.png" width="180" height="400" loading="lazy" alt="Student home with the live attendance banner, upcoming lessons and attendance overview">
<figcaption>Student home</figcaption>
</figure>
<figure>
<img src="/images/projects/attendease/student_calendar.png" width="180" height="400" loading="lazy" alt="Month calendar with lesson dots and today circled">
<figcaption>Calendar</figcaption>
</figure>
<figure>
<img src="/images/projects/attendease/lecturer_lectures.png" width="180" height="400" loading="lazy" alt="Module lesson list opened at today's lesson with Start Attendance">
<figcaption>Lecturer's lessons</figcaption>
</figure>
</div>

## How it is built

- **MVVM over Room.** Five tables (users, modules, lessons, enrolments and
  attendance). Fragments observe LiveData from ViewModels and never touch
  the database directly.
- **No polling.** The DAOs return LiveData, so when a lecturer opens a
  session, the banner on the student's screen appears by itself.
- **Testing without NFC.** On a phone or emulator without NFC, the scanner
  shows a Simulate card tap button. Simulated taps run through the same code
  as real ones.
- **Demo data that stays current.** The demo timetable is rebuilt around the
  current date, so there is always a lesson today to try.

## Try it

[Open the browser demo](https://appetize.io/app/b_wpwek676k67podfwefczyucynu)
and press play. Log in as lecturer `1000001` with password `password`, open
**Lectures**, expand **50.001 Information Systems** and tap
**Start Attendance**. The demo can be unavailable near the end of a month if
its free minutes run out.

On an Android phone, install the
[APK](https://github.com/ngzhankang/AttendEase/releases/latest) and log in the
same way. With an NFC phone you can link any transit or office card to a
demo student and tap it.
