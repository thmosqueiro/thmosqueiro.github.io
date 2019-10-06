# My personal website

This is the repository I have for my personal website, where I have everything
I work with + a blog.

## Personal setup

To test locally, I use the [jekyll/jekyll docker image](https://github.com/envygeeks/jekyll-docker/blob/master/README.md):
```
$ export JEKYLL_VERSION=XXXXX
$ docker container run --name blog \
  --volume="/path/to/website/source/:/srv/jekyll" \
  -p 4000:4000 -it jekyll/jekyll:$JEKYLL_VERSION \
  jekyll serve --watch --drafts
```
