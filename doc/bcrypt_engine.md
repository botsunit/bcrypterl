

# Module bcrypt_engine #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#autodetect_cost-1">autodetect_cost/1</a></td><td>
Autodetects the cost from the salt string.</td></tr><tr><td valign="top"><a href="#calibrate-1">calibrate/1</a></td><td>
Returns the cost factor which will result in computation times less than <tt>Limit</tt> ms.</td></tr><tr><td valign="top"><a href="#generate_salt-0">generate_salt/0</a></td><td>Equivalent to <a href="#generate_salt-1"><tt>generate_salt(DEFAULT_COST)</tt></a>.</td></tr><tr><td valign="top"><a href="#generate_salt-1">generate_salt/1</a></td><td>
Generates a random salt with a given computational cost.</td></tr><tr><td valign="top"><a href="#hash_secret-2">hash_secret/2</a></td><td>
Given a secret and a valid salt (see bcrypt_engine:generate_salt/1) calculates
a bcrypt() password hash.</td></tr><tr><td valign="top"><a href="#valid_salt-1">valid_salt/1</a></td><td>
Returns true if <tt>Salt</tt> is a valid bcrypt() salt, false if not.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="autodetect_cost-1"></a>

### autodetect_cost/1 ###


<pre><code>
autodetect_cost(Salt::string()) -&gt; integer() | {error, invalid_salt}
</code></pre>
<br />


Autodetects the cost from the salt string.
<a name="calibrate-1"></a>

### calibrate/1 ###


<pre><code>
calibrate(Limit::integer()) -&gt; integer()
</code></pre>
<br />


Returns the cost factor which will result in computation times less than `Limit` ms.
<a name="generate_salt-0"></a>

### generate_salt/0 ###


<pre><code>
generate_salt() -&gt; string() | {error, any()}
</code></pre>
<br />

Equivalent to [`generate_salt(DEFAULT_COST)`](#generate_salt-1).
<a name="generate_salt-1"></a>

### generate_salt/1 ###


<pre><code>
generate_salt(Cost::integer()) -&gt; string() | {error, any()}
</code></pre>
<br />


Generates a random salt with a given computational cost.
<a name="hash_secret-2"></a>

### hash_secret/2 ###


<pre><code>
hash_secret(Secret::string(), Salt::string()) -&gt; string() | {error, any()}
</code></pre>
<br />


Given a secret and a valid salt (see bcrypt_engine:generate_salt/1) calculates
a bcrypt() password hash.
<a name="valid_salt-1"></a>

### valid_salt/1 ###


<pre><code>
valid_salt(Salt::string()) -&gt; true | false
</code></pre>
<br />


Returns true if `Salt` is a valid bcrypt() salt, false if not.
