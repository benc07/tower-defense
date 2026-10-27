# Team Contract: Quadruple Towers

## Communication and meetings

We will communicate through text messages, reply very often (under 3 hours), and organize meetings as needed, ideally once a week for a few hours. If one of us can't attend the meeting, they will let the others know via text if they can't attend, at least one day in advance.

## Decisions

Anyone can propose an idea in the team channel or at a meeting. Before we decide on an idea, everyone gets to say their view, either in the meeting or by writing it in the channel.

When people disagree, we will aim for a consensus. If we don't reach a consensus after a short discussion, we vote and the majority wins. If the vote is tied, the person whose area is most affected by the decision breaks the tie.

Design questions that affect more than one module (types, interfaces, file formats) get decided by the whole team. Choices inside a single person's module are decided by that module's owner.

To reach decisions in a timely manner, we will enforce a deadline. If a question is still open after 48 hours, we vote so nobody is blocked.

## Commitments and problems

At the end of each weekly meeting, everyone states what they will finish before the next one in order to track progress.

If someone is stuck or likely to miss a commitment, they tell the team channel as soon as they know, at least 24 hours before the due date when possible. They say what is blocking them and what help they need.

When work is missed, we talk about it within 24 hours. We agree on a new date, or reassign the task to the backup owner, and check it again at the next meeting.

If someone misses commitments repeatedly after we've tried everything else, we ask the professor for team coaching right away.

## Review and integration

Before every push, the author runs `dune build` and `dune test` and fixes any failures or bugs. They also run `git pull` first so they push on top of the latest code.

The author reads through any AI-generated code before pushing and must be able to explain how it works. We don't push any code nobody has looked at.

Commit messages should be detailed and say exactly what changed.

AI-written tests get their expected answers checked by hand, independent of the implementation, before they are trusted.

After a push, the backup owner of that module reads the change within a few days and flags anything unclear or wrong in the team channel.

Backup owners stay familiar with the code by reading the .mli file and tests of the module they back up. Once or twice during the project, the primary owner gives a short walkthrough of their module.

Any change to a shared interface (an .mli file or a shared type) gets announced in the team channel before it is pushed, saying what changed and which modules it affects. Teammates who use that interface confirm it still works with their code.

If someone breaks the build on main, they fix it right away, and everyone else holds off on pushing new code until it's green.

## AI use

All of us plan to use AI to supplement our work across the project, including design, implementation, tests, and documentation. We have no differences in expectation, so there are no individual restrictions.

We will use AI responsibly and all agree that we should integrate it into our project development as needed and do not anticipate significant disagreements.

We ask AI for changes in small pieces at a time so a teammate can follow each one.

Each primary owner must be able to explain how their module works. If they can't, they will walk through the code with the team to get a better understanding of it or simplify their code until they can.

If someone is uncomfortable with how AI was used, they raise their concerns in the channel or at the next meeting. We decide by team vote, using the same process as other decisions. If the concern involves a course rule, we follow the stricter reading and ask the professor or staff.

We follow the course AI and academic-integrity policy. We are responsible for the correctness of what we submit and must be able to explain it.

We will describe our AI use honestly in the MS2 and MS3 reports, with specific examples.

## Revisiting the agreement

We check the contract at three set points: the first team meeting after MS1 is submitted, right after MS2 is submitted (when we also fill out the peer evaluation forms), and about halfway between MS2 and MS3.

At each check, we go through the contract section by section and ask whether it is working. We specifically look at response times, missed commitments, whether pushes to main are staying green, and whether AI-assisted code is actually being reviewed.

Anyone can also call for an early check at any time by posting in the team channel. We hold it at the next meeting, or sooner if it is urgent.

To change something, the person proposes the new wording, everyone gets a chance to respond, and then we decide using our standard decision process (consensus, then a vote if needed).

We update the contract file in the repo right away and post a short note in the channel saying what changed, so everyone is working from the same version.