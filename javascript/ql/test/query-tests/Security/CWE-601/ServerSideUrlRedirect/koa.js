const Koa = require('koa');
const url = require('url');
const app = new Koa();

app.use(async ctx => {
	var url = ctx.query.target;
	ctx.redirect(url); // NOT OK
	ctx.redirect(`${url}${x}`); // NOT OK

	var isCrossDomainRedirect = url.parse(url || '', false, true).hostname;
	if(!url || isCrossDomainRedirect) {
		ctx.redirect('/'); // OK
	} else {
		ctx.redirect(url); // NOT OK
	}

	if(!url || isCrossDomainRedirect || ! url.match(VALID)) {
		ctx.redirect('/'); // OK
	} else {
		ctx.redirect(url); // OK
	}
});

app.listen(3000);
