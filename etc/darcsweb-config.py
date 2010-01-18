
# base configuration, common to all repos
class base:
	# location of the darcs logo
	darcslogo = "../darcsweb/darcs.png"

	# location of the darcs favicon
	darcsfav = "../darcsweb/minidarcs.png"

	# the CSS file to use
	cssfile = '../darcsweb/style.css'

	# this script's name, usually just "darcsweb.cgi" unless you rename
	# it; if you leave this commented it will be detected automatically
	#myname = "darcsweb.cgi"

	# our url, used only to generate RSS links, without the script name;
	# if you leave this commented it will be detected automatically
	#myurl = "http://example.com/darcsweb"

	# optionally, you can specify the path to the darcs executable; if you
	# leave this commented, the one on $PATH will be used (this is
	# normally what you want)
	#darcspath = "/home/me/bin/"

	# the text to appear in the top of repo list; this is also optional,
	# and html-formatted
	#summary = "I love darcs!"

	# in case you want to change the beautiful default, you can specify an
	# alternative footer here; it's optional, of course
	#footer = "I don't like shoes"

	# It is possible to have a cache where darcsweb will store the pages
	# it generates; entries are automatically updated when the repository
	# changes. This will speed things up significatively, specially for
	# popular sites.
	# It's recommended that you clean the directory with some regularity,
	# to avoid having too many unused files. A simple rm will do just
	# fine.
	# If you leave the entry commented, no cache will be ever used;
	# otherwise the directory is assumed to exist and be writeable.
	# If you use this option you must set the "myname" and "myurl"
	# variables.
	#cachedir = '/tmp/darcsweb-cache'

	# By default, darcsweb's search looks in the last 100 commits; you can
	# change that number by specifying it here.
	# Note that search are not cached, so if you have tons of commits and
	# set the limit to a very high number, they will take time.
	#searchlimit = 100

	# If you want to log the times it took darcsweb to present a page,
	# uncomment this option. The value should be a file writeable by
	# darcsweb.
	#logtimes = "/tmp/darcsweb_times"

	# If you want darcsweb to automatically detect embedded URLs,
	# define them here, using python-style regexps like the examples
	# below. They will be replaced in summaries, logs, and commits.
	# The following variables are replaced:
	#   myreponame: repository link (darcsweb.cgi?r=repo)
	#   reponame: repository name (repo)
	#
	#url_links = (
	  # Format is: (regexp, replacement)
	  # Some examples:
	  #
	  # Detect '#NNN' as a reference to bug database
	  #(r'#([0-9]+)',
	  # 	r'<a href="/bugs/show_bug.cgi?id=\1">#\1</a>'),
	  #
	  # Replace hashes with commit-links.
	  #(r'(\d{14}-[0-9a-f]{5}-[0-9a-f]{40}\.gz)',
	  #	r'<a href="%(myreponame)s;a=commit;h=\1">\1</a>'),
	#)

	# If you want to generate links from patch author names, define the url
	# here. Example for CIA:
	#author_links = "http://cia.navi.cx/stats/author/%(author)s"

	# If you want to disable the annotate feature (for performance reasons,
	# the http connection will time out on slow machines), uncomment this
	# option.
	#disable_annotate = True



# debian default configuration to enable auto-magic for repositories
# in /var/www/darcs

class head:
        multidir = '/opt/darcs/'
        repodesc = 'Repositories with the latest changes'
 	autodesc = True
        repourl = 'http://dwim.hu/darcs/%(name)'
 	repoencoding = "utf8", "ascii", "latin1"
        exclude = 'ebr42', 'nafi'

class live:
        multidir = '/opt/hu.dwim.home/workspace/'
        repodesc = 'Repositories that were used to compile the currently running server at <a href="http://dwim.hu">dwim.hu</a>'
 	autodesc = True
        repourl = 'http://dwim.hu/live-workspace/%(name)'
 	repoencoding = "utf8", "ascii", "latin1"

# class multi1:
# 	multidir = '/usr/local/src'
# 	#multidir_deep = False
# 	repodesc = 'Repository for %(name)s'
# 	repourl = 'http://example.com/repos/%(name)s/'
# 	repoencoding = 'latin1'

# 	# optional, see above
# 	#repoprojurl = 'http://example.com/projects/%(name)s/'

# 	# if you want to exclude some directories, add them to this list (note
# 	# they're relative to multidir, not absolute)
# 	#exclude = 'dir1', 'dir2'

# 	# if you want the descriptions to be picked up automatically from the
# 	# file named "_darcs/third_party/darcsweb/desc" (one line only), set
# 	# this to True. It defaults to False
# 	#autodesc = True

# 	# if you want to exclude all the repositories which do NOT have a
# 	# directory named "_darcs/third_party/darcsweb/" inside, set this to
# 	# True. It defaults to False.
# 	#autoexclude = True

# 	# if you want urls to be picked up automatically from the file named
# 	# "_darcs/third_party/darcsweb/url" (one line only), set this to
# 	# True. It defaults to False.
# 	#autourl = True

# 	# if you want the projects urls to be picked up automatically from the
# 	# file named "_darcs/third_party/darcsweb/extdoc" (one line only), set
# 	# this to True. It defaults to False.
# 	#autoprojurl = True
