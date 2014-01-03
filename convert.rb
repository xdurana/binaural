#!/usr/bin/env ruby

require 'rubygems'
require 'json'
require 'csv'

class EncephalAppTest

    attr_accessor :items
    attr_accessor :accumulated

    def initialize(stroop_off_test, stroop_off, stroop_on_test, stroop_on)
        @stroop_off_test, @stroop_off, @stroop_on_test, @stroop_on = stroop_off_test, stroop_off, stroop_on_test, stroop_on
        @items = []
        @accumulated = []
    end

    def float(value)
        return "%.3f" % value.to_f
    end

	def read(subject)
        
        id = subject['id']
        age = subject['age']
        gender = subject['gender']
        sequence = subject['sequence']

		sequence.split("").each_with_index do |sound, order|            
			CSV.foreach("raw/#{id}#{sequence[order]}.csv", :headers => true) do |row|
	            ["off", "on"].each_with_index do |level, i|
	                for trial in (1..5) do
	                    index = row.index "Stroop #{level} run #{trial} components"
	                    for column in (index+1..index+10) do
	                        @items << "#{id},#{age},#{gender},#{sound},#{level},#{sequence},#{trial},#{column - index},#{float(row[column])}"
	                    end
	                end
	            end
                @accumulated << "#{id},#{age},#{gender},#{sound},off,#{sequence},#{row['OffTime']},#{row['Total # of runs stroop off'].to_i}"
                @accumulated << "#{id},#{age},#{gender},#{sound},on,#{sequence},#{row['OnTime']},#{row['Total # of runs stroop on'].to_i}"
			end
		end
	end
end

class Experiment
    def initialize(file)
        stroop_test = EncephalAppTest.new(2,5,2,5)
        subjects = JSON.parse(File.read(file));
        subjects.each do |subject|
            if subject['ok'] == 'Y'
                stroop_test.read(subject)
            end
        end
        File.open("R/items.csv", "w+") do |f|
            f.puts "subject,age,gender,sound,level,sequence,trial,item,time"
            f.puts stroop_test.items
        end
        File.open("R/conditions.csv", "w+") do |f|
            f.puts "subject,age,gender,sound,level,sequence,time,runs"
            f.puts stroop_test.accumulated
        end
    end
end

experiment = Experiment.new('experiment.json')