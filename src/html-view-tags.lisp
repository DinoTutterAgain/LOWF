(defpackage :lowf.html-view.tags
  (:use :cl)
  (:export :comment
	   :doctype
	   :a
	   :abbr
	   :acronym
	   :address
	   :applet
	   :area
	   :article
	   :aside
	   :audio
	   :b
	   :base
	   :basefont
	   :bdi
	   :bdo
	   :big
	   :blockquote
	   :body
	   :br
	   :button
	   :canvas
	   :caption
	   :center
	   :cite
	   :code
	   :col
	   :colgroup
	   :data
	   :datalist
	   :dd
	   :del
	   :details
	   :dfn
	   :dialog
	   :dir
	   :div
	   :dl
	   :dt
	   :em
	   :embed
	   :fieldset
	   :figcaption
	   :figure
	   :font
	   :footer
	   :form
	   :frame
	   :frameset
	   :head
	   :header
	   :hgroup
	   :h1
	   :h2
	   :h3
	   :h4
	   :h5
	   :h6
	   :hr
	   :html
	   :i
	   :iframe
	   :img
	   :input
	   :ins
	   :kbd
	   :label
	   :legend
	   :li
	   :link
	   :main
	   :tmap
	   :mark
	   :menu
	   :meta
	   :meter
	   :nav
	   :noframes
	   :noscript
	   :object
	   :ol
	   :optgroup
	   :option
	   :output
	   :p
	   :param
	   :picture
	   :pre
	   :progress
	   :q
	   :rp
	   :rt
	   :ruby
	   :s
	   :samp
	   :script
	   :tsearch
	   :section
	   :select
	   :small
	   :source
	   :span
	   :strike
	   :strong
	   :style
	   :sub
	   :summary
	   :sup
	   :svg
	   :table
	   :tbody
	   :td
	   :template
	   :textarea
	   :tfoot
	   :th
	   :thead
	   :ttime
	   :title
	   :tr
	   :track
	   :tt
	   :u
	   :ul
	   :var
	   :video
           :wbr))
  
(in-package :lowf.html-view.tags)

(defmacro doctype (type)
  `(list :doctype ,type))

(defmacro comment (message)
  `(list :comment ,message))

(defmacro a ((&rest args) &body children)
  `(list :a (list ,@args) ,@children))

(defmacro abbr ((&rest args) &body children)
  `(list :abbr (list ,@args) ,@children))

(defmacro address ((&rest args) &body children)
  `(list :address (list ,@args) ,@children))

(defmacro area (&rest args)
  `(list :area (list ,@args)))

(defmacro article ((&rest args) &body children)
  `(list :article (list ,@args) ,@children))

(defmacro aside ((&rest args) &body children)
  `(list :aside (list ,@args) ,@children))

(defmacro audio ((&rest args) &body children)
  `(list :audio (list ,@args) ,@children))

(defmacro b ((&rest args) &body children)
  `(list :b (list ,@args) ,@children))

(defmacro base (&rest args)
  `(list :base (list ,@args)))

(defmacro bdi ((&rest args) &body children)
  `(list :bdi (list ,@args) ,@children))

(defmacro bdo ((&rest args) &body children)
  `(list :bdo (list ,@args) ,@children))

(defmacro blockquote ((&rest args) &body children)
  `(list :blockquote (list ,@args) ,@children))

(defmacro body ((&rest args) &body children)
  `(list :body (list ,@args) ,@children))

(defmacro br (&rest args)
  `(list :br (list ,@args)))

(defmacro button ((&rest args) &body children)
  `(list :button (list ,@args) ,@children))

(defmacro canvas ((&rest args) &body children)
  `(list :canvas (list ,@args) ,@children))

(defmacro caption ((&rest args) &body children)
  `(list :caption (list ,@args) ,@children))

(defmacro cite ((&rest args) &body children)
  `(list :cite (list ,@args) ,@children))

(defmacro code ((&rest args) &body children)
  `(list :code (list ,@args) ,@children))

(defmacro col ((&rest args) &body children)
  `(list :col (list ,@args) ,@children))

(defmacro colgroup ((&rest args) &body children)
  `(list :colgroup (list ,@args) ,@children))

(defmacro data ((&rest args) &body children)
  `(list :data (list ,@args) ,@children))

(defmacro datalist ((&rest args) &body children)
  `(list :datalist (list ,@args) ,@children))

(defmacro dd ((&rest args) &body children)
  `(list :dd (list ,@args) ,@children))

(defmacro del ((&rest args) &body children)
  `(list :del (list ,@args) ,@children))

(defmacro details ((&rest args) &body children)
  `(list :details (list ,@args) ,@children))

(defmacro dfn ((&rest args) &body children)
  `(list :dfn (list ,@args) ,@children))

(defmacro dialog ((&rest args) &body children)
  `(list :dialog (list ,@args) ,@children))

(defmacro div ((&rest args) &body children)
  `(list :div (list ,@args) ,@children))

(defmacro dl ((&rest args) &body children)
  `(list :dl (list ,@args) ,@children))

(defmacro dt ((&rest args) &body children)
  `(list :dt (list ,@args) ,@children))

(defmacro em ((&rest args) &body children)
  `(list :em (list ,@args) ,@children))

(defmacro embed (&rest args)
  `(list :embed (list ,@args)))

(defmacro fieldset ((&rest args) &body children)
  `(list :fieldset (list ,@args) ,@children))

(defmacro figcaption ((&rest args) &body children)
  `(list :figcaption (list ,@args) ,@children))

(defmacro figure ((&rest args) &body children)
  `(list :figure (list ,@args) ,@children))

(defmacro footer ((&rest args) &body children)
  `(list :footer (list ,@args) ,@children))

(defmacro form ((&rest args) &body children)
  `(list :form (list ,@args) ,@children))

(defmacro head ((&rest args) &body children)
  `(list :head (list ,@args) ,@children))

(defmacro h1 ((&rest args) &body children)
  `(list :h1 (list ,@args) ,@children))

(defmacro h2 ((&rest args) &body children)
  `(list :h2 (list ,@args) ,@children))

(defmacro h3 ((&rest args) &body children)
  `(list :h3 (list ,@args) ,@children))

(defmacro h4 ((&rest args) &body children)
  `(list :h4 (list ,@args) ,@children))

(defmacro h5 ((&rest args) &body children)
  `(list :h5 (list ,@args) ,@children))

(defmacro h6 ((&rest args) &body children)
  `(list :h6 (list ,@args) ,@children))

(defmacro header ((&rest args) &body children)
  `(list :header (list ,@args) ,@children))

(defmacro hgroup ((&rest args) &body children)
  `(list :hgroup (list ,@args) ,@children))

(defmacro hr (&rest args)
  `(list :hr (list ,@args)))

(defmacro html ((&rest args) &body children)
  `(list :html (list ,@args) ,@children))

(defmacro i ((&rest args) &body children)
  `(list :i (list ,@args) ,@children))

(defmacro iframe ((&rest args) &body children)
  `(list :iframe (list ,@args) ,@children))

(defmacro img (&rest args)
  `(list :img (list ,@args)))

(defmacro input (&rest args)
  `(list :input (list ,@args)))

(defmacro ins ((&rest args) &body children)
  `(list :ins (list ,@args) ,@children))

(defmacro kbd ((&rest args) &body children)
  `(list :kbd (list ,@args) ,@children))

(defmacro label ((&rest args) &body children)
  `(list :label (list ,@args) ,@children))

(defmacro legend ((&rest args) &body children)
  `(list :legend (list ,@args) ,@children))

(defmacro li ((&rest args) &body children)
  `(list :li (list ,@args) ,@children))

(defmacro link (&rest args)
  `(list :link (list ,@args)))

(defmacro main ((&rest args) &body children)
  `(list :main (list ,@args) ,@children))

(defmacro tmap ((&rest args) &body children)
  `(list :map (list ,@args) ,@children))

(defmacro mark ((&rest args) &body children)
  `(list :mark (list ,@args) ,@children))

(defmacro menu ((&rest args) &body children)
  `(list :menu (list ,@args) ,@children))

(defmacro meta (&rest args)
  `(list :meta (list ,@args)))

(defmacro meter ((&rest args) &body children)
  `(list :meter (list ,@args) ,@children))

(defmacro nav ((&rest args) &body children)
  `(list :nav (list ,@args) ,@children))

(defmacro noscript ((&rest args) &body children)
  `(list :noscript (list ,@args) ,@children))

(defmacro object ((&rest args) &body children)
  `(list :object (list ,@args) ,@children))

(defmacro ol ((&rest args) &body children)
  `(list :ol (list ,@args) ,@children))

(defmacro optgroup ((&rest args) &body children)
  `(list :optgroup (list ,@args) ,@children))

(defmacro option ((&rest args) &body children)
  `(list :option (list ,@args) ,@children))

(defmacro output ((&rest args) &body children)
  `(list :output (list ,@args) ,@children))

(defmacro p ((&rest args) &body children)
  `(list :p (list ,@args) ,@children))

(defmacro param (&rest args)
  `(list :param (list ,@args)))

(defmacro picture ((&rest args) &body children)
  `(list :picture (list ,@args) ,@children))

(defmacro pre ((&rest args) &body children)
  `(list :pre (list ,@args) ,@children))

(defmacro progress ((&rest args) &body children)
  `(list :progress (list ,@args) ,@children))

(defmacro q ((&rest args) &body children)
  `(list :q (list ,@args) ,@children))

(defmacro rp ((&rest args) &body children)
  `(list :rp (list ,@args) ,@children))

(defmacro rt ((&rest args) &body children)
  `(list :rt (list ,@args) ,@children))

(defmacro ruby ((&rest args) &body children)
  `(list :ruby (list ,@args) ,@children))

(defmacro s ((&rest args) &body children)
  `(list :s (list ,@args) ,@children))

(defmacro samp ((&rest args) &body children)
  `(list :samp (list ,@args) ,@children))

(defmacro script ((&rest args) &body children)
  `(list :script (list ,@args) ,@children))

(defmacro tsearch ((&rest args) &body children)
  `(list :search (list ,@args) ,@children))

(defmacro section ((&rest args) &body children)
  `(list :section (list ,@args) ,@children))

(defmacro select ((&rest args) &body children)
  `(list :select (list ,@args) ,@children))

(defmacro small ((&rest args) &body children)
  `(list :small (list ,@args) ,@children))

(defmacro source (&rest args)
  `(list :source (list ,@args)))

(defmacro span ((&rest args) &body children)
  `(list :span (list ,@args) ,@children))

(defmacro strong ((&rest args) &body children)
  `(list :strong (list ,@args) ,@children))

(defmacro style ((&rest args) &body children)
  `(list :style (list ,@args) ,@children))

(defmacro sub ((&rest args) &body children)
  `(list :sub (list ,@args) ,@children))

(defmacro summary ((&rest args) &body children)
  `(list :summary (list ,@args) ,@children))

(defmacro sup ((&rest args) &body children)
  `(list :sup (list ,@args) ,@children))

(defmacro svg ((&rest args) &body children)
  `(list :svg (list ,@args) ,@children))

(defmacro table ((&rest args) &body children)
  `(list :table (list ,@args) ,@children))

(defmacro tbody ((&rest args) &body children)
  `(list :tbody (list ,@args) ,@children))

(defmacro td ((&rest args) &body children)
  `(list :td (list ,@args) ,@children))

(defmacro template ((&rest args) &body children)
  `(list :template (list ,@args) ,@children))

(defmacro textarea ((&rest args) &body children)
  `(list :textarea (list ,@args) ,@children))

(defmacro tfoot ((&rest args) &body children)
  `(list :tfoot (list ,@args) ,@children))

(defmacro th ((&rest args) &body children)
  `(list :th (list ,@args) ,@children))

(defmacro thead ((&rest args) &body children)
  `(list :thead (list ,@args) ,@children))

(defmacro ttime ((&rest args) &body children)
  `(list :time (list ,@args) ,@children))

(defmacro title ((&rest args) &body children)
  `(list :title (list ,@args) ,@children))

(defmacro tr ((&rest args) &body children)
  `(list :tr (list ,@args) ,@children))

(defmacro track (&rest args)
  `(list :track (list ,@args)))

(defmacro u ((&rest args) &body children)
  `(list :u (list ,@args) ,@children))

(defmacro ul ((&rest args) &body children)
  `(list :ul (list ,@args) ,@children))

(defmacro var ((&rest args) &body children)
  `(list :var (list ,@args) ,@children))

(defmacro video ((&rest args) &body children)
  `(list :video (list ,@args) ,@children))

(defmacro wbr (&rest args)
  `(list :wbr (list ,@args)))
