# Minio

Official minio documentation available: [https://docs.min.io/](https://docs.min.io/)

## Minio philosophy

Minio clusters are configured with a fixed size once, and cannot be resized
afterwards. It is thus important to make a good conservative estimate about
the amount of
data you are going to store.  The original philosophy was a set-up-and-forget
strategy.  You set up N disks and then you have a N/2 redundancy. Once half of
your disks are dead (or a bit earlier) it is time to set up a new cluster and
migrate everything using Minio's mirroring facilities. Of course this is not
ideal because the longer you have your cluster, the less space it will be able
to reliably store.  Hence minio has gotten some healing capabilities, which we
will cover how to use in the next section. It is possible to replace broken
disks with fixed disks, and then heal.

However, it is not possible to shrink or increase the cluster size. If the
cluster is starting to get full, you will need to set up a parallel bigger
cluster, mirror everything to the new cluster, swap the DNS entries to
the new one, and then decommission the old one.

## Hurdles from the trenches: disk usage statistics; directories vs. disks

I have done some more go code reading and have solved more minio
mysteries.  tl;dr: if you want to be safe, run minio on disks, not
mounted directories.

Minio will detect if a directory is a mount point and if it is call `statfs(2)`
to figure out the amount of available blocks.  If it's not a mount directory,
it will just call `du .` in a for loop and update some counter (which sounds
like a bad strategy to me).

<https://github.com/minio/minio/blob/e6d8e272ced8b54872c6df1ef2ad556092280224/cmd/posix.go#L320-L352>

so the answer is: if you use minio, e.g. with mountpoints, it will silently do
the right thing and if you configure it to use two directories on the same
mount, it will silently do something slightly incorrect (rather than crash loudly).
(`du` is not a very reliable way to get Used metrics.)

So the Used metric in `mc info` is lying. We should instead look at the `Total` and `Available`
metric which are derived from `statfs`.  Those are already exposed by node-exporter anyway.

Moral of the story: we're probably getting weird numbers because we're not
using disks but we're using folders instead. We should use disks.

When doing a healing procedure, minio will give you feedback per bucket item.
If a bucket item happens to be say 500 Megabytes, it might look like the progress
is stuck, but it just means it doesn't update until the entire 500mb file is
healed. Don't worry and have a bit of patience.
