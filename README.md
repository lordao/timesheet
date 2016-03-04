Timesheet
=========

Usage:

timesheet < date 'YYYY-MM-DD'> < user > < repo >

All arguments are optional

According to [Wikipedia](https://en.wikipedia.org/wiki/Timesheet "Timesheet"):

>A timesheet (or time sheet) is a method for recording the amount of a
>worker's time spent on each job.

This (toy) project "records" commits rather than time. By giving it a date
("YYYY-MM-DD" format) and part of the author's name in Git it will search the
current directory's repository and print all their commits.

Where I work we need to register what we've done during the day, but after
a whole day of work I'd constantly forget the details, and would simply
copy-paste my commit messages. Since I was feeling lazy, I decided to make
a script.

Maybe I'll someday make it compute time spent between commits, estimate which
ones were harder to make, and other statistics. But it's good for now.
