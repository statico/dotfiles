guard 'rspec', :version => 2, :bundler => true, :cli => ' --color', :all_on_start =>true     do
  watch(%r{^spec/(.*).rb$}) { |m| m[0] }
  watch(%r{^lib/(.*).rb$}) do |m|
    "spec/#{m[0].split('/').last.split('.').first.downcase}_spec.rb"
  end
end
